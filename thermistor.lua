-- thermistor.lua
-- NodeMCU thermistor module for the ESP8266.

-- -------------------------------------------------------------------
-- AUTHOR:	Patrick Dorion <pau-github@outlook.com>
--
-- VERSION:	0.0.1-RELEASE (cb60365dfc9684d80c552246bd1e1ea88d34c7d9)
--
-- DATE:	Jul. 26, 2019.
--
-- LICENSE: You must obtain permission from the author to use, or
--			or reproduce this code in any way.
-- -------------------------------------------------------------------


--	CIRCUIT DIAGRAM
--
--	[+]--[THERMISTOR]--[ADC]--[RES]--[-]


module ( 'thermistor', package.seeall )

dofile ( 'thermistor.conf' )

function thermistor.log ( x )

	--[[
	--		Returns the natural logarithm of the provided value.
	--
	--		math.log() can also be enabled in the NodeMCU build options, but isn't by default.
	--
	--		x			number		The parameter-value.
	--
	--		Returns:	number		The natural logarithm of the provided value.
	--
	--		Provided by: Egor Skriptunoff (https://stackoverflow.com/questions/52579137).
	]]--

   assert ( x > 0 )

   local a, b, c, d, e, f = x < 1 and x or 1 / x, 0, 0, 1, 1

   repeat
      repeat
         c, d, e, f = c + d, b * d / e, e + 1, c
      until c == f
      b, c, d, e, f = b + 1 - a * c, 0, 1, 1, b
   until b <= f

   return a == x and -f or f

end  --  End of function.


function thermistor.initADC ( mode )

	--[[
	--		This function initialized the ADC on the ESP8266EX.  This is mostly in case you want
	--		to read the interval voltage reference to make more exact calculations.
	--
	--		mode		string	Either 'adc' or 'vcc'.
	--
	--		Returns:	nothing.
	--
	--		Depends on	msg (module).
	]]--


   if ( mode == nil ) then

      msg.log ( 'No ADC mode specified (Analog Converter or Voltmeter).  Using default (ADC).', 'thermistor.lua, initADC()', 'debug' )

      mode = 'ADC'

   end

	if ( string.upper ( mode ) == 'ADC' ) then

		if ( adc.force_init_mode ( adc.INIT_ADC ) == true ) then
			msg.log ( 'ADC set to read internal voltage reference, need to set to external read.  Rebooting...', 'thermistor.lua, initADC()', 'info' )
			node.restart ( )
		end

	elseif ( string.upper ( mode ) == 'VCC' ) then

		if ( adc.force_init_mode ( adc.INIT_VDD33 ) == true ) then
			msg.log ( 'ADC set to read external value, need to set to internal voltage reference.  Rebooting...', 'thermistor.lua, initADC()', 'info' )
			node.restart ( )
		end

	end

end  --  End of function.


function thermistor.getResistance ( )

    --[[
    --		Returns the resistance of the thermistor.
    --
    --		Returns:	number          The resistance of the thermistor.
    --
    --		Depends:	module			msg;
    --                  global          _G['thermistor']['volref'];
    --                  global          _G['thermistor']['resdiv'].
    ]]--


	local Vref = _G['thermistor']['volref']
	local R1 = _G['thermistor']['resdiv']

	local Vr1, Vr2, R2, I

	Vr1 = adc.read ( 0 ) / 1024 * Vref
	I = Vr1 / R1
	Vr2 = Vref - Vr1
	R2 = Vr2 / I

	msg.log ( 'R1 = ' .. R1 .. ' ADC = ' .. adc.read ( 0 ) .. ' Vr1 = ' .. Vr1 .. ' I = ' .. I .. ' Vr2 = ' .. Vr2 .. ' R2 = ' .. R2 , 'thermistor.lua, getResistance()', 'debug' )

	return R2

end  --  End of function.


function thermistor.getBetaParameters ( res )

	--[[
	--		Returns the temperatures and resistances used to calculate the Beta.
	--
	--		res		    	number	    The resistance of the thermistor.
	--
	--		Returns:		number		Temperature 1, in degrees Kelvin;
	--						number		Resistance 1;
	--						number		Temperature 2, in degrees Kelvin;
	--						number		Resistance 2.
	--
	--		Depends:		module      msg;
	--						global      _G['thermistor']['table'].
	]]--


	local tbl = _G['thermistor']['table']

	local diff, mink, minv, maxk, maxv, maxd
	local mind = tbl[1][2]

	for key, value in pairs ( tbl ) do

		diff = math.abs ( value[2] - res )

		if ( diff <= mind ) then
			mind = diff
			minv = value[2]
			mink = key
		end

	end

	msg.log ( 'Min Val = ' .. minv, 'thermistor.lua, getBetaParameters()', 'debug' )

	if ( res < tbl[mink - 1][2] and res >= minv ) then
		maxk = mink - 1
		maxv = tbl[maxk][2]
		maxd = math.abs ( res - maxv )
	end

	if ( res > tbl[mink + 1][2] and res <= minv ) then
		maxk = mink + 1
		maxv = tbl[maxk][2]
		maxd = math.abs ( res - maxv )
	end

	msg.log ( 'Max val = ' .. maxv, 'thermistor.lua, getBetaParameters()', 'debug' )

	return tbl[mink][1] + 273.15, minv, tbl[maxk][1] + 273.15, maxv

end  --  End of function.


function thermistor.getBeta ( T1, R1, T2, R2 )

	--[[
	--		Returns the Beta value to be used as coefficient in the Steinhart-Hart method.
	--
	--		T1			number		Temperature 1, in degrees Kelvin.
	--		R1			number		Resistance 1.
	--		T2			number		Temperature 2, in degrees Kelvin.
	--		R2			number		Resistance 2.
	--
	--		Returns:	number		Steinhart-Hart Beta value.
	--
	--		Depends:    module      msg.
	]]--


	local B = thermistor.log ( R2 / R1 ) / ( 1 / T2 - 1 / T1 )

	msg.log ( 'B = ' .. B, 'thermistor.lua, getBeta()', 'debug' )

	return B

end  --  End of function.


function thermistor.getABCParameters ( res, method )

	--[[
	--		Selects an interval of temperatures to calculate Steinhart-Hart ABC coefficient.
	--
	--		These intervals were manually determined in a spreadsheet.
	--
	--		res			number		The measured resistance of the thermistor.
	--		method		string		Either 'GEO', 'LOG', or 'PROX'.
	--
	--		Returns:	number		Temperature A,
	--					number		Resistance A;
	--					number		Temperature B,
	--					number		Resistance B;
	--					number		Temperature C,
	--					number		Resistance C.
	--
	--		Depends:    module      msg;
    --                  global      _G['thermistor']['table'].
	]]--


	local tbl = _G['thermistor']['table']

	local mind, mink, diff, minv, interval, TA, RA, TB, RB, TC, RC = tbl[1][2]

	for key, value in pairs ( tbl ) do

		diff = math.abs ( value[2] - res )

		if ( diff <= mind ) then
			mind = diff
			minv = value[1]
			mink = key
		end

	end

	msg.log ( 'Min V = ' .. minv, 'thermistor.lua, getABCParameters()', 'debug' )

	if ( string.upper ( method ) == 'GEO' ) then

		for k, v in pairs ( _G['thermistor']['geointervals'] ) do
			if ( minv >= v[1] and minv < v[2] ) then
				interval = k
			end
		end

		msg.log ( 'Interval = ' .. interval, 'thermistor.lua, getABCParameters()', 'debug' )

		for k2, v2 in pairs ( tbl ) do

			if ( _G['thermistor']['geointervals'][interval][1] == v2[1] ) then
				TA = v2[1] + 273.15
				RA = v2[2]
			elseif ( _G['thermistor']['geointervals'][interval][3] == v2[1] ) then
				TB = v2[1] + 273.15
				RB = v2[2]
			elseif ( _G['thermistor']['geointervals'][interval][2] == v2[1] ) then
				TC = v2[1] + 273.15
				RC = v2[2]
			end

		end

	elseif ( string.upper ( method ) == 'LOG' ) then

		for k, v in pairs ( _G['thermistor']['logintervals'] ) do
			if ( minv >= v[1] and minv < v[2] ) then
				interval = k
			end
		end

		msg.log ( 'Interval = ' .. interval, 'thermistor.lua, getABCParameters()', 'debug' )

		for k2, v2 in pairs ( tbl ) do

			if ( _G['thermistor']['logintervals'][interval][1] == v2[1] ) then
				TA = v2[1] + 273.15
				RA = v2[2]
			elseif ( _G['thermistor']['logintervals'][interval][3] == v2[1] ) then
				TB = v2[1] + 273.15
				RB = v2[2]
			elseif ( _G['thermistor']['logintervals'][interval][2] == v2[1] ) then
				TC = v2[1] + 273.15
				RC = v2[2]
			end

		end

	elseif ( string.upper ( method ) == 'PROX' ) then

		if ( mink < table.maxn ( tbl ) and mink > 1 ) then
			TA = tbl[mink - 1][1] + 273.15
			RA = tbl[mink - 1][2]
			TB = tbl[mink][1] + 273.15
			RB = tbl[mink][2]
			TC = tbl[mink + 1][1] + 273.15
			RC = tbl[mink + 1][2]
		elseif ( mink == 1 ) then
			TA = tbl[mink][1] + 273.15
			RA = tbl[mink][2]
			TB = tbl[mink + 1][1] + 273.15
			RB = tbl[mink + 1][2]
			TC = tbl[mink + 2][1] + 273.15
			RC = tbl[mink + 2][2]
		elseif ( mink == table.maxn ( tbl ) ) then
			TA = tbl[mink - 2][1] + 273.15
			RA = tbl[mink - 2][2]
			TB = tbl[mink - 1][1] + 273.15
			RB = tbl[mink - 1][2]
			TC = tbl[mink][1] + 273.15
			RC = tbl[mink][2]
		end

	end

	msg.log ( 'TA, RA, TB, RB, TC, RC = ' .. ( TA .. ' ' .. RA .. ' ' .. TB .. ' ' .. RB .. ' ' .. TC .. ' ' .. RC ), 'thermistor.lua, getABCParameters()', 'debug' )

	return TA, RA, TB, RB, TC, RC

end  --  End of function.


function thermistor.getABC ( T1, R1, T2, R2, T3, R3 )

	--[[
	--		This function returns the Steinhart-Hart A, B, C coefficients
	--
	--		T1			number		Temperature 1, in degrees Kelvin.
	--		R1			number		Resistance 1.
	--		T2			number		Temperature 2, in degrees Kelvin.
	--		R2			number		Resistance 2.
	--		T3			number		Temperature 3, in degrees Kelvin.
	--		R3			number		Resistance 3.
	--
	--		Returns:	number		The A coefficient;
	--					number		the B coefficient; and
	--					number		the C coefficient.
	--
	--		Depends:    module      msg.
	]]--


	local L1, L2, L3, Y1, Y2, Y3, gamma2, gamma3, C, B, A

	L1 = thermistor.log ( R1 )
	L2 = thermistor.log ( R2 )
	L3 = thermistor.log ( R3 )
	Y1 = 1 / T1
	Y2 = 1 / T2
	Y3 = 1 / T3
	gamma2 = (Y2 - Y1) / (L2 - L1)
	gamma3 = (Y3 - Y1) / (L3 - L1)
	C = (gamma3 - gamma2) / (L3 - L2) * (L1 + L2 + L3)^-1
	B = gamma2 - C * (L1^2 + (L1 * L2) + L2^2)
	A = Y1 - L1 * (B + (C * L1)^2)

	msg.log ( 'A = ' .. A .. ' B = ' .. B .. ' C = ' .. C, 'thermistor.lua, getABC', 'debug' )

	return A, B, C

end  --  End of function.


function thermistor.getTemperature ( scale, method, abcmethod )

	--[[
	--		Returns the temperature from a thermistor in either degrees C.
	--
	--		scale		string		Either 'K', 'C', or 'F'; defaults to Kelvin.
	--		method	    string		Either 'ABC', 'Beta', or 'BetaMfg'; defaults to 'ABC'.
	--		abcmethod   string		Either 'LOG', 'GEO', or 'PROX'; defaults to 'GEO'.
	--
	--		Returns:	number		The temperature.
	--
	--		Depends:    module      msg;
    --                  [global]    _G['thermistor']['scale'];
    --                  [global]    _G['thermistor']['method']
    --                  [global]    _G['thermistor']['abcmethod']
	]]--


	if ( scale == nil ) then scale = _G['thermistor']['scale'] end
	if ( scale == nil ) then scale = 'K' end
	if ( method == nil ) then method = _G['thermistor']['method'] end
	if ( method == nil ) then method = 'ABC' end
	if ( abcmethod == nil ) then abcmethod = _G['thermistor']['abcmethod'] end
	if ( abcmethod == nil ) then abcmethod = 'GEO' end

	msg.log ( 'Scale = ' .. scale .. ' Method = ' .. method .. ' ABC Method = ' .. abcmethod, 'thermistor.lua, getTemperature()', 'debug' )

	local Rt, T

	thermistor.initADC ( 'adc' )

	Rt = thermistor.getResistance ( )

	if ( string.upper ( method ) == 'ABC' ) then

		local A, B, C = thermistor.getABC ( thermistor.getABCParameters ( Rt, abcmethod ) )
		T = 1 / ( A + (B * thermistor.log ( Rt )) + (C * thermistor.log ( Rt ))^3)
		msg.log ( 'ABC T = ' .. T - 273.15, 'thermistor.lua, getTemperature()', 'debug' )

	elseif ( string.upper ( method ) == 'BETA' ) then

		local T1, R1, T2, R2 = thermistor.getBetaParameters ( Rt )
		local B = thermistor.getBeta ( T1, R1, T2, R2 )

		T = ((1 / B) * thermistor.log (Rt / R1) + 1 / T1)^-1

	elseif ( string.upper ( method ) == 'BETAMFG' ) then

		msg.log ( 'Mfg Beta = ' .. _G['thermistor']['mfgbeta'], 'thermistor.lua, getTemperature()', 'debug' )

		local T1, R1, T2, R2 = thermistor.getBetaParameters ( Rt )
		local B = _G['thermistor']['mfgbeta']

		T = ((1 / B) * thermistor.log (Rt / R1) + 1 / T1)^-1

	end

	if ( string.upper ( scale ) == 'C' ) then

		T = T - 273.15

	elseif ( string.upper ( scale ) == 'F' ) then

		T = 1.8 * T - 459.67

	end

	msg.log ( 'T = ' .. T, 'thermistor.lua, getTemperature()', 'debug' )

	return T

end  --  End of function.


return thermistor


-- End of File
