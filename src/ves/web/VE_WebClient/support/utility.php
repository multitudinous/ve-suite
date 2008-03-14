<?php

	// All functions written by Ken Kopecky. Function descriptions (not comments
	// within functions) written by Ben Harper.




	// Function to get an integer living at the end of a string.

	function getAppendedNum($str, $length)
	{	
		$slen=strlen($str);
		if ( $slen < 4)
		{
			echo ("getAppendedNum:  string '$str' is too short!<BR>\n");
			return 0;
		}
		$num="";
		//grab the last $length characters of the string (the number)
		for($i = $length; $i > 0; $i--)
		{
			$num .= $str{$slen-$i};					//tack this digit onto the beginning of our number		
		}
		//echo $num;
		$num += 0;	//convert to int;	 		
	
		return $num;
	}




	// Connect to the database.
	
//	function databaseConnect()
//	{
//		include_once("configuration.php");
//	}




	// Parses string to get file extension (text after the last dot).

	function getFileExtension($fileName)
	{
		$len=strlen($fileName);
		$ext="";
		//grab the last few characters of the string 
		for($i=0; $i<$len; $i++)
		{
			$ext.=$fileName{$i};	 
			if($fileName{$i}==".")
				$ext="";
		}			
		return $ext;
	}




	// Arrays of (X, Y) coordinates of inputs/outputs (on diagram) to components.
	// Unfortunately, this is hardcoded. It needs to be in the database.

	function makeTypeArray()
	{
		global $types;
		
		$types['GasSource']['ports']['out'][]=array('x'=>53, 'y'=>22);
		$types['GasSource']['image']="gas_source.gif";
		
		$types['V21ASU']['ports']['in'][]=array('x'=>1, 'y'=>36);
		$types['V21ASU']['ports']['out'][]=array('x'=>53, 'y'=>36);
		$types['V21ASU']['image']="ASU.gif";
		
		$types['Compressor']['ports']['in'][]=array('x'=>9, 'y'=>26);
		$types['Compressor']['ports']['out'][]=array('x'=>43, 'y'=>74);
		$types['Compressor']['image']="compressor.gif";
		
		$types['Gasifier0D']['ports']['in'][]=array('x'=>36, 'y'=>118);
		$types['Gasifier0D']['ports']['out'][]=array('x'=>36, 'y'=>18);
		$types['Gasifier0D']['image']="gasifier2.gif";
		
		$types['Cyclone']['ports']['in'][]=array('x'=>2, 'y'=>9);
		$types['Cyclone']['ports']['out'][]=array('x'=>12, 'y'=>49);
		$types['Cyclone']['image']="cyclone.gif";
		
		$types['HeatExchanger']['ports']['in'][]=array('x'=>6, 'y'=>13);
		$types['HeatExchanger']['ports']['in'][]=array('x'=>6, 'y'=>32);
		$types['HeatExchanger']['ports']['out'][]=array('x'=>52, 'y'=>13);
		$types['HeatExchanger']['ports']['out'][]=array('x'=>52, 'y'=>32);
		$types['HeatExchanger']['image']="heat_exchanger.gif";
		
		$types['WaterSource']['ports']['in'][]=array('x'=>6, 'y'=>22);
		$types['WaterSource']['ports']['out'][]=array('x'=>52, 'y'=>22);
		$types['WaterSource']['image']="water_source.gif";
		
		$types['GasMixer']['ports']['in'][]=array('x'=>7, 'y'=>12);
		$types['GasMixer']['ports']['in'][]=array('x'=>7, 'y'=>19);
		$types['GasMixer']['ports']['in'][]=array('x'=>7, 'y'=>26);
		$types['GasMixer']['ports']['in'][]=array('x'=>7, 'y'=>33);
		$types['GasMixer']['ports']['out'][]=array('x'=>53, 'y'=>24);
		$types['GasMixer']['image']="gas_mixer.gif";
		
		$types['ChlorineBed']['ports']['in'][]=array('x'=>18, 'y'=>82);
		$types['ChlorineBed']['ports']['out'][]=array('x'=>18, 'y'=>22);
		$types['ChlorineBed']['image']="chlorinebed.gif";
		
		$types['BulkDesulfurizer']['ports']['in'][]=array('x'=>13, 'y'=>77);
		$types['BulkDesulfurizer']['ports']['out'][]=array('x'=>58, 'y'=>82);
		$types['BulkDesulfurizer']['image']="desulfurizer.gif";
		
		$types['SulfurPolisher']['ports']['in'][]=array('x'=>24, 'y'=>23);
		$types['SulfurPolisher']['ports']['out'][]=array('x'=>24, 'y'=>78);
		$types['SulfurPolisher']['image']="polisher.gif";
		
		$types['GasSplitter']['ports']['in'][]=array('x'=>6, 'y'=>21);
		$types['GasSplitter']['ports']['out'][]=array('x'=>52, 'y'=>11);
		$types['GasSplitter']['ports']['out'][]=array('x'=>52, 'y'=>18);
		$types['GasSplitter']['ports']['out'][]=array('x'=>52, 'y'=>26);
		$types['GasSplitter']['ports']['out'][]=array('x'=>52, 'y'=>33);
		$types['GasSplitter']['image']="gas_splitter.gif";
		
		$types['SOFC']['ports']['in'][]=array('x'=>17, 'y'=>65);
		$types['SOFC']['ports']['in'][]=array('x'=>91, 'y'=>22);
		$types['SOFC']['ports']['out'][]=array('x'=>64, 'y'=>68);
		$types['SOFC']['image']="sofc.gif";
		
		$types['Recuperator']['ports']['in'][]=array('x'=>13, 'y'=>45);
		$types['Recuperator']['ports']['in'][]=array('x'=>44, 'y'=>23);
		$types['Recuperator']['ports']['out'][]=array('x'=>77, 'y'=>45);
		$types['Recuperator']['ports']['out'][]=array('x'=>45, 'y'=>70);
		$types['Recuperator']['image']="recuperator.gif";
		
		$types['SteamTurbine']['ports']['in'][]=array('x'=>5, 'y'=>17);
		$types['SteamTurbine']['ports']['out'][]=array('x'=>76, 'y'=>17);
		$types['SteamTurbine']['image']="steamturbine.gif";
		
		$types['GasFeedback']['ports']['in'][]=array('x'=>10, 'y'=>4);
		$types['GasFeedback']['ports']['in'][]=array('x'=>6, 'y'=>22);
		$types['GasFeedback']['ports']['out'][]=array('x'=>52, 'y'=>22);
		$types['GasFeedback']['image']="gas_feedback.gif";
		
		$types['EquilibriumReactor']['ports']['in'][]=array('x'=>7, 'y'=>20);
		$types['EquilibriumReactor']['ports']['out'][]=array('x'=>41, 'y'=>66);
		$types['EquilibriumReactor']['image']="equilibrium_reactor.gif";
		
		$types['GasHeatExchanger']['ports']['in'][]=array('x'=>7, 'y'=>17);
		$types['GasHeatExchanger']['ports']['out'][]=array('x'=>27, 'y'=>17);
		$types['GasHeatExchanger']['image']="heat_exchanger_gas.gif";
	}




	// Draws arrows at the end of lines (on diagram).

	function drawArrow(&$pic, $x1, $y1, $x2, $y2, $size, $color)
	{
		$width=0.5*$size;
		$length=1.5*$size;
		$vector['x']=$x2-$x1;
		$vector['y']=$y2-$y1;
		$vector['length']=sqrt($vector['x']*$vector['x']+$vector['y']*$vector['y']);
		$vector['x']/=$vector['length'];
		$vector['y']/=$vector['length'];
		$perp['x']=$vector['y'];
		$perp['y']=-$vector['x'];
		$verts[]=$x2;
		$verts[]=$y2;
		$verts[]=$x2-$vector['x']*$length+$perp['x']*$width;
		$verts[]=$y2-$vector['y']*$length+$perp['y']*$width;
		$verts[]=$x2-$vector['x']*$length-$perp['x']*$width;
		$verts[ ]=$y2-$vector['y']*$length-$perp['y']*$width;
		imageFilledPolygon($pic, $verts, 3, $color); 
	}




	// Not sure what this function does.

	function checkExclusions($name)
	{
		return 0;
		$i=0;
		$excludeList=array("XPOS", "YPOS", "m_xUserScale", "m_yUserScale");
		foreach($excludeList as $xxxx)
			$i+=($name==$xxxx);
		return $i;
	}




	// Not sure what this function does.

	function removeChar($char, $string)
	{
		$temps = explode($char, $string);
		$string = "";
		foreach($temps as $word)
		{
			$string .= $word;
		}
		return $string;
	}




	// Reads image map data and prints it.

	function insertMapData()
	{
		$fp=@fopen("data/localMap.html", "r");
		if(!$fp) echo ("error reading map data file.<BR>\n");
		else $mapString=@fread($fp, fileSize("data/localMap.html"));
		fclose($fp);
		echo $mapString;
	}

?>
