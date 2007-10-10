<?php

	// Primary data manager for web interface.

	include_once("module.php");
	include_once("link.php");
	include_once("configuration.php");

	class PPManager
	{
		var $modules;
		var $links;
		var $layoutModule;
		var $ID;
		var $successfulDownload;
	
		function PPManager($ID)
		{
			makeTypeArray();
			$this->ID = $ID;
			$this->successfulDownload = false;

			$this->downloadData();
			$this->updateImage();
			if($this->successfulDownload)
				$this->uploadFullData();
		}




		// Downloads components and sets them up.

		function downloadData()
		{	
			global $project_name;
			unset($this->modules);
			$qel = "select * from " . $project_name . "data where ID = '$this->ID'";
			$result = mysql_query($qel);
			$moduleData = mysql_fetch_array($result);
			$dataString = $moduleData['data'];
			$moduleStrings = explode("%%%%", $dataString);
			foreach($moduleStrings as $moduleString)
			{
				$temp = explode("&&&", $moduleString);
				$ID = $temp[0];
				$this->modules[$ID] = new PPModule($moduleString);
			}
		
			if(isset($this->modules[-1]))
			{
				$this->layoutModule = $this->modules[-1];
				$this->successfulDownload = true;
			}
			foreach($this->layoutModule->strings as $name => $type)
			{
				$num = getAppendedNum($name, 4);		//module IDs don't always match up with the array ID number
				$this->modules[$num]->setInfo("", $num, $type);		//set the (currently empty) name, ID number, and type
			}	
			$this->createLinks();
		}




		// Upload component data to the database.

		function uploadData()
		{
			foreach($this->modules as $module)
			{
				$module->uploadData();
			}
		}




		// Create links to components.

		function createLinks()
		{
			foreach($this->layoutModule->ints as $name => $thisInt)
			{
				if(strpos($name, "ln_")!==false)
				{	
					$str = $name;
					$num=getAppendedNum($str, 4);
					if(!isset($this->links[$num]))
					{
						$this->links[$num] = new PPLink;
					}
					// figure out what kind of link value it is and act accordingly
					if(strpos($str, "ln_FrMod")!==false)
						$this->links[$num]->fromModID = $thisInt;
				
					elseif(strpos($str, "ln_FrPort") !== false)
						$this->links[$num]->fromPort = $thisInt;

					elseif(strpos($str, "ln_ToMod") !== false)
						$this->links[$num]->toModID = $thisInt;
					
					elseif(strpos($str, "ln_ToPort") !== false)
						$this->links[$num]->toPort = $thisInt;

				}
			}

			//now look through intarrays for connectors (elbows)
			$vertSets = array();					///save an array of vertices in a more usable format first
			foreach($this->layoutModule->intArrays as $name => $thisInt)
			{
				if(strpos($name, "ln_")!==false) 			//check if this is a link value
				{	
					$str=$name;
					$num=getAppendedNum($str, 4);	
					//figure out what kind of link value it is (x or y elbow) and act accordingly
					if(strpos($str, "ln_ConX")!==false)
						foreach($thisInt as $value)
						{
							$vertSets[$num]['x'][]=$value;
							//$imageKey+=$value;	
						}
						
					if(strpos($str, "ln_ConY")!==false)
						foreach($thisInt as $value)
						{
							$vertSets[$num]['y'][]=$value;
							//$imageKey+=$value;	
						}
				}
			}
		
			//go through all the links and add a starting vertex
			foreach($this->links as $tag => $link)
			{
				$pos = $this->modules[$link->fromModID]->getOutPos($link->fromPort);
				$this->links[$tag]->addVertex($pos['x'], $pos['y']);
			}
			
			//now actually add them to our links
			foreach ($vertSets as $num => $set)
			{
				foreach($set['x'] as $index => $dontcare)		//we just need to use the link index here for adding it
				{
					$this->links[$num]->addVertex($set['x'][$index], $set['y'][$index]);		//this is the part we assume they're in the correct order
				}
			}
			
			//go through all the links and add an ending vertex
			foreach($this->links as $tag=> $link)
			{
				$pos = $this->modules[$link->toModID]->getInPos($link->toPort);
				$this->links[$tag]->addVertex($pos['x'], $pos['y']);
			}
		}
	
		function updateImage()
		{
			ini_set("memory_limit","20M");
			global $picDataString;
			global $imageKey;	
			global $types;
			makeTypeArray();
		
			$layoutPic=imagecreateTrueColor(1500, 1350);
			//make the picture white
			$white = ImageColorAllocate($layoutPic, 255, 255, 255);
			$black = ImageColorAllocate($layoutPic, 0, 0, 0);
			imageFilledRectangle($layoutPic,0, 0, 1600, 1400, $white);
		
			foreach($this->modules as $tag => $module)
			{
				//echo ("$tag<BR>");
				if($tag != "-1")
					$this->modules[$tag]->draw($layoutPic, $black);
			}
		
			imageSetThickness($layoutPic, 1);
			foreach($this->links as $link)
			{
				$link->draw($layoutPic, $black);
			}
		
			imagejpeg($layoutPic, "images/componentOverview.jpg");
			imagedestroy($layoutPic);
			$this->writeMapFile();
		}
	
		function writeMapFile()				///note:  this MUST be called after the draw functin, so components know their proper pixel size
		{
			$mapString = ("<map name=\"componentMap\">\n");
			$lmk = "overview.php";

			foreach($this->modules as $module)
			{
				$module->addImageMapTarget($mapString, $lmk);
			}
		
			$mapString.=("</map>");

			$fp=fopen("data/localMap.html", "w");
			if(!$fp) echo ("PPManager:  error writing map data file.<BR>\n");
			else fwrite($fp, $mapString);
			fclose($fp);
		}

		function makePullDownMenu()
		{
			//make a form for choosing our component
			$is_selected = "";
			$lmk=$_SERVER['PHP_SELF'];
			$pullDownGenString = "\n<center><p><hr size=\"1\" noshade color=\"#000000\"><form action=\"overview.php\" method=\"submit\">\n<input type=\"hidden\" name=\"locate\" value=\"components\">\n<select name=\"component\">\n";
			$pullDownGenString .= "<option></option>";
			foreach($this->modules as $module)
			{	
			
				if ( ! ( $module->ID == "" || $module->ID == "-1" ) ) { 
					$pullDownGenString .= "<option value=\"$module->ID\"";
					//if this is the current component, add a selected tag
					$targetID = 0;
					if(isset($_GET['component'])) 
						$targetID = $_GET['component'];
					else if(isset($_POST['component']))
						$targetID = $_POST['component'];
					if($module->ID == $targetID)
						$pullDownGenString .= " selected";
					$pullDownGenString .= ">$module->ID: $module->type</option>\n";
				}
			}

			$pullDownGenString .= "</select>&nbsp;&nbsp;<input type=\"hidden\" name=\"locate\" value=\"components\"><INPUT target=\"components\" TYPE=\"SUBMIT\" VALUE=\"Go\" name=\"force_update\">";
			if ( $targetID ) { $pullDownGenString.= "</p></form><p><form action=\"javascript:regenImage();\"><input type=\"submit\" onclick=\"javascript:regenImage();\" value=\"Update Component Diagram (Forced)\"><hr size=\"1\" noshade color=\"#000000\">\n</form></p></center><p>&nbsp;</p>\n"; }
			else { $pullDownGenString.= "<hr size=\"1\" noshade color=\"#000000\"></p></form>"; }
			return $pullDownGenString;
		}

		function makeSelectionMenu()
		{
			$is_selected = "";
			$menuString = "\n<select name=\"components[]\" size=\"4\" multiple>\n";
			foreach($this->modules as $module) {
			
				if ( ! ( $module->ID == "" || $module->ID == "-1" || $module->ID == "100000" ) ) {
				$menuString .= "<option value=\"$module->ID\"";
					//if this is the current component, add a selected tag
					$targetID = 0;
					if(isset($_GET['component'])) 
						$targetID = $_GET['component'];
					else if(isset($_POST['component']))
						$targetID = $_POST['component'];
					if($module->ID == $targetID)
						$menuString .= " selected";
					$menuString .= ">$module->ID: $module->type</option>\n";
				}

			}

			$menuString .= "</select>";
			return $menuString;
		}
	
		function uploadFullData()
		{	
			global $project_name;
			if(!$this->successfulDownload) return;			//if we didnn't successfully downloada the data, return
			$moduleData = "";								///cat all the strings together into a huge huge thing
			foreach($this->modules as $module)
			{
				$moduleData .= $module->getSQLString() . "%%%%";
			}
			if(strlen($moduleData) < 128)
			{
				echo("Error!  Trying to upload invalid configuration data.  aborting\n");
				return;		
			}
			
			$qel = "delete from " . $project_name . "data where ID = '$this->ID'";
			mysql_query($qel);
			$t = time();
	//		$qel = "insert into " . $project_name . "data set ID = $this->ID, timestamp = '$t', data = '$moduleData'";
			$qel = "insert into " . $project_name . "data set ID = $this->ID, data = '$moduleData'";
			mysql_query($qel);
		}
	}

?>