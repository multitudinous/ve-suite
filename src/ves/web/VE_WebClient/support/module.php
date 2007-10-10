<?

	// All functions written by Ken Kopecky. Function descriptions (not comments
	// within functions) written by Ben Harper.




	// Web equivelent of VE-Suite's user interface. (Ken)




	include_once("utility.php");
	include_once("configuration.php");
	error_reporting(9999);

	class PPModule
	{
		var $ints;										// Stored integers
		var $intString;									// SQL data string used for the database
	
		var $doubles;
		var $doubleString;
	
		var $strings;
		var $stringString;
	
		var $intArrays;
		var $intArrayString;
	
		var $doubleArrays;
		var $doubleArrayString;
	
		var $stringArrays;
		var $stringArrayString;
	
		var $ID;

		var $type;
		var $name;
		var $sizeX;										// Size in pixels
		var $sizeY;

		function PPModule($dataString)
		{
			$this->init();
			$strings = explode("&&&", $dataString);
			$this->ID = $strings[0];
			if(!isset($strings[6]))							// If we didn't get enough strings, this is a dud
				return;
			$this->intString = $strings[1];
			$this->doubleString = $strings[2];
			$this->stringString = $strings[3];
			$this->intArrayString = $strings[4];
			$this->doubleArrayString = $strings[5];
			$this->stringArrayString = $strings[6];
			$this->parseSQLStrings();
			$this->processModifications();
		}
	
		function setInfo($name, $ID, $type)
		{
			$this->name = $name;
			// $this->ID = $ID;
			$this->type = $type;
		}
	
		function init()
		{
			$this->ints[] = array();
			$this->doubles[] = array();
			$this->strings[] = array();
			$this->intArrays[] = array();
			$this->doubleArrays[] = array();
			$this->stringArrays = array();
			$this->ints['XPOS'] = 0;
			$this->ints['YPOS'] = 0;
		}




		// Parses SQL strings (queries).

		function parseSQLStrings()
		{
			unset($this->ints);
			$vals1 = explode("||", $this->intString);					// Explode the string into value sets
			foreach($vals1 as $dataset)
			{	
				if($dataset != "")
				{
					$vals = explode("|", $dataset);
					if(isset($vals[1]))
					$this->ints[$vals[0]] =  $vals[1];
				}
			}
		
			unset($this->doubles);	
			$vals1 = explode("||", $this->doubleString);				// Explode the string into value sets
			foreach($vals1 as $dataset)
			{	
				if($dataset != "")
				{
					$vals = explode("|", $dataset);
					if(isset($vals[1]))
					{
						$this->doubles[$vals[0]] =  $vals[1];
					}
					else
					{
						$this->doubles[$vals[0]] = 0;
					}
				}
			}
	
	
			unset($this->strings);
			$vals1 = explode("||", $this->stringString);				// Explode the string into value sets
			foreach($vals1 as $dataset)
			{	
				if($dataset != "")
				{
					$vals = explode("|", $dataset);
					$this->strings[$vals[0]] =  $vals[1];
				}
			}

			unset($this->intArrays);
			$vals1 = explode("||", $this->intArrayString);				// Explode the string into value sets
			foreach($vals1 as $dataset)
			{	
				if($dataset != "")
				{
					$vals = explode("|", $dataset);				
					foreach($vals as $key => $thisValue)
					{
						if($key == '0')						// The first key is the name
						{
							$index = $thisValue;
						}
						else
						{
							$this->intArrays[$index][] = $thisValue;
						}
					}
				}
			}
		
			unset($this->doubleArrays);
			$vals1 = explode("||", $this->doubleArrayString);			// Explode the string into value sets
			foreach($vals1 as $dataset)
			{	
				if($dataset != "")
				{
					$vals = explode("|", $dataset);				
					foreach($vals as $key => $thisValue)
					{
						if($key == '0')						// The first key is the name
						{
							$index = $thisValue;
						}
						else
						{
							$this->doubleArrays[$index][] = $thisValue;
						}
					}
				}
			}
		
			unset($this->stringArrays);
			$vals1 = explode("||", $this->stringArrayString);			// Explode the string into value sets
			foreach($vals1 as $dataset)
			{	
				if($dataset != "")
				{
					$vals = explode("|", $dataset);				
					foreach($vals as $key => $thisValue)
					{
						if($key == '0')						// The first key is the name
						{
							$index = $thisValue;
						}
						else
						{
							$this->stringArrays[$index][] = $thisValue;
						}
					}
				}
			}
		}




		// Builds SQL queries.

		function buildSQLStrings()
		{
			$SQLString = "";
			foreach($this->ints as $name => $thisData)
			{
				$SQLString .= "$name|$thisData||";					// Separate with pipes
			}
		
			$this->intString = $SQLString;
		
			$SQLString = "";
			if(isset($this->doubles))
			foreach($this->doubles as $name => $thisData)
			{
				$SQLString .= "$name|$thisData||";					// Separate with pipes
			}
			$this->doubleString = $SQLString;
		
			$SQLString = "";
			if(isset($this->strings))
			foreach($this->strings as $name => $thisData)
			{
				$SQLString .= "$name|$thisData||";					// Separate with pipes
			}

			$this->stringString = $SQLString;
		
			$SQLString = "";
			if(isset($this->intArrays))
			foreach($this->intArrays as $name => $thisData)
			{
				$SQLString .="$name|" . implode("|", $thisData) . "||";
			}
		
			$this->intArrayString = $SQLString;
		
			$SQLString = "";
			if(isset($this->doubleArrays))
			foreach($this->doubleArrays as $name => $thisData)
			{
				$SQLString .="$name|" . implode("|", $thisData) . "||";
			}

			$this->doubleArrayString = $SQLString;
		
			$SQLString = "";
			if(isset($this->stringArrays))
			foreach($this->stringArrays as $name => $thisData)
			{
				$SQLString .="$name|" . implode("|", $thisData) . "||";
			}

			$this->stringArrayString = $SQLString;
		}




		// Draws image map graphics/text.

		function draw(&$image, $textColor)
		{
			global $types;
			if(!(isset($this->type)))
				return;
			$img=$types[$this->type]['image'];
			$insert=imagecreatefromgif("images/$img");
			imagecolortransparent($insert, imagecolorexact($insert, 255, 255, 255));
			$this->sizeX = imagesx($insert);						// Get size here, cuz we need it later
			$this->sizeY = imagesy($insert);
			$x = $this->ints['XPOS'];
			$y = $this->ints['YPOS'];
			imagecopymerge($image, $insert,$x, $y, 0, 0, $this->sizeX, $this->sizeY, 100);
			ImageTTFText($image, 8, 0, $x, $y, $textColor,"data/MetaSans.ttf", $this->ID);	
		}




		// Add components to the image map.

		function addImageMapTarget(&$mapString, $baseLink)
		{			
			if(!(isset($this->type)))							// If this doesn't have a type, it's not drawable and shouldn't be on the map
				return;
			//image map data so the user can click on it and it will link to this component
			$x = $this->ints['XPOS'];
			$y = $this->ints['YPOS'];
			$mapString.=("<area target=\"components\" href=\"$baseLink"."?page=components&component=" .
			$this->ID."\" shape=\"rect\" coords=\"$x, $y, ".
			($x + $this->sizeX) . ", " . ($y+$this->sizeY) . "\">\n");
		}




		// Get position ((X, Y) coordinates) of component outputs.

		function getOutPos($num)
		{
			global $types;
			$pos = array();
			$pos['x'] = $this->ints['XPOS'];
			$pos['y'] = $this->ints['YPOS'];
			if(!isset($types[$this->type]['ports']['out'][$num]['x']))
			{
				$num = 0;
			}
			$pos['x'] += $types[$this->type]['ports']['out'][$num]['x'];
			$pos['y'] += $types[$this->type]['ports']['out'][$num]['y'];
		
			return $pos;
		}




		// Get position ((X, Y) coordinates) of component inputs.

		function getInPos($num)
		{	
			global $types;
			$pos = array();
			$pos['x'] = $this->ints['XPOS'];
			$pos['y'] = $this->ints['YPOS'];
			if(!isset($types[$this->type]['ports']['in'][$num]['x']))
			{
				$num = 0;
			}
			$pos['x'] += $types[$this->type]['ports']['in'][$num]['x'];
			$pos['y'] += $types[$this->type]['ports']['in'][$num]['y'];
		
			return $pos;
		}




		// Creates table showing data members associated with the selected component.
		// Also creates the form for modifying data members.

		function drawTable()
		{
			global $types;
			global $project_name;
			$tableGenString = "";
			$img=$types[$this->type]['image'];
			if($img!="")
			$tableGenString.="<center><table cellpadding=\"0\" cellspacing=\"0\" border=\"0\">\n";
			$tableGenString.="<tr><td align=\"center\"><img src=\"images/$img\"></td><td width=\"20\"></td>";
			$tableGenString .= "<form action=\"overview.php?page=components\" method=\"post\">";
			$tableGenString .= "<td>ID: $this->ID<BR>\nType: $this->type\n";

			/* if($this->type == "EquilibriumReactor")
			{
				$x = $this->ints['XPOS'];
				$y = $this->ints['YPOS'];
				echo "pos:  $x, $y<BR>";
		
			} */

			$tableGenString .= "</td></tr></table></center><p>&nbsp;</p>";
			$tableGenString .= "<center><hr size=\"1\" noshade color=\"#000000\">Data Members<hr size=\"1\" noshade color=\"#000000\">\n";
			$tableGenString .= "<table width=\"250\" cellspacing=\"3\" cellpadding=\"0\" border=\"0\">\n";
			$tableGenString .= "<tr><td><strong>Name</strong></td><td align=\"center\"><strong>Value(s)</strong></td></tr>";

			if(isset($this->doubles))
			foreach($this->doubles as $tag => $pair)
				if(!checkExclusions($tag))
				{
					$dString = "";
					$dString = sprintf( "%2.2f", $pair);
					$tableGenString .=  "<td>$tag</td><td align=\"center\"><input type=\"text\" size=\"12\" name=\"$tag\" value=\"$dString\"></td></tr>\n";
				}

			if(isset($this->strings))
			foreach($this->strings as $tag => $pair)
				if(!checkExclusions($tag))
				{
					$tableGenString .=  "<td>$tag</td><td align=\"center\"><input type=\"text\" size=\"12\" name=\"$tag\" value=\"$pair\"></td></tr>\n";
				}
			
			if(isset($this->ints))
			foreach($this->ints as $tag => $pair)
				if(!checkExclusions($tag))
				{
					$tableGenString .=  "<td>$tag</td><td align=\"center\"><input type=\"text\" size=\"12\" wrap=\"virtual\" name=\"$tag\" value=\"$pair\"></td></tr>\n";
				}
		
			if(isset($this->intArrays))
			foreach($this->intArrays as $tag => $set)
			{       
				$valueString = implode(", ", $set);
				$tableGenString .= "<td>$tag</td><td align=\"center\"><textarea rows=\"4\" cols=\"15\" wrap=\"virtual\" name=\"$tag\">$valueString</textarea></td></tr>\n";
			}
		
			if(isset($this->stringArrays))
			foreach($this->stringArrays as $tag => $set)
			{       
				$valueString = implode(", ", $set);
				$tableGenString .= "<td valign=\"top\">$tag</td><td align=\"center\"><textarea rows=\"4\" cols=\"15\" wrap=\"virtual\" name=\"$tag\">$valueString</textarea>";
				$tableGenString .= "</td></tr>\n";
			}
		
			if(isset($this->doubleArrays))
			foreach($this->doubleArrays as $tag => $set)
			{       
				$valueString = implode(", ", $set);
				$tableGenString .= "<td valign=\"top\">$tag</td><td align=\"center\"><textarea rows=\"4\" cols=\"15\" wrap=\"virtual\" name=\"$tag\">$valueString</textarea>";
				$tableGenString .= "</td></tr>\n";
			}
		
			$tableGenString.="</table></center>";
			$tableGenString .= "<input type=\"hidden\" value=\"$this->ID\" name=\"component\">";
			$tableGenString .= "<input type=\"hidden\" value=\"components\" name=\"locate\">";
			$tableGenString .= "<hr size=\"1\" noshade color=\"#000000\"><table width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\"><tr><td align=\"right\"><input type=\"submit\" value=\"Submit Changes\" name=\"modifyModule\"></td></tr></table></form>\n";

			$tableGenString .= "<p>&nbsp;</p>\n";
			$tableGenString .= "<p>&nbsp;</p>\n";
			$tableGenString .= "<center><hr size=\"1\" noshade color=\"#000000\">Associated Files<hr size=\"1\" noshade color=\"#000000\">\n";
			$tableGenString .= "<table width=\"100%\" cellspacing=\"3\" cellpadding=\"0\" border=\"0\">\n";
		
			$query = "SELECT * FROM ".$project_name."files WHERE components LIKE '%$this->ID%'";
			$result = mysql_query($query);
		
			$assoc_component = mysql_fetch_assoc($result);
			if ( $assoc_component ) {
				$id = $assoc_component["id"];
				$filename = $assoc_component["filename"];
				$tableGenString .= "<tr><td valign=\"middle\" width=\"16\" height=\"18\"><a href=\"files_view.php?id=$id\" target=\"file_view\"><img src=\"graphics/file_icon.gif\" width=\"16\" height=\"18\" align=\"middle\" border=\"0\"></a></td>";
				$tableGenString .= "<td valign=\"middle\"><a href=\"files_view.php?id=$id\" target=\"file_view\" style=\"font-family: Verdana, Tahoma, sans-serif; font-size: 11px; font-weight: normal; text-decoration: underline;\">$filename</a></td></tr>";
			
				while ( $assoc_component = mysql_fetch_assoc($result) )
				{ 
					$id = $assoc_component["id"];
					$filename = $assoc_component["filename"];
					$tableGenString .= "<tr><td valign=\"middle\" width=\"16\" height=\"18\"><a href=\"files_view.php?id=$id\" target=\"file_view\"><img src=\"graphics/file_icon.gif\" width=\"16\" height=\"18\" align=\"middle\" border=\"0\"></a></td>";
					$tableGenString .= "<td valign=\"middle\"><a href=\"files_view.php?id=$id\" target=\"file_view\" style=\"font-family: Verdana, Tahoma, sans-serif; font-size: 11px; font-weight: normal; text-decoration: underline;\">$filename</a></td></tr>";
				}
			
			} else { $tableGenString .= "<tr><td>There are no files associated with this component.</tr></td>"; }
			
			$tableGenString .= "</table>\n";
			$tableGenString .= "</center>\n";

			$fp=fopen("myComponent.html", "w");
			if(!$fp) 
				echo ("PPModule:  Error writing component data file.<BR>\n");
			else 
				fwrite($fp, $tableGenString);
			return $tableGenString;
		}




		// Apply changes to components made by user.

		function processModifications()
		{
			if(!isset($_POST['modifyModule']))
				return;
			if($_POST['component'] != $this->ID)
				return;
			//loop through and see what's changed
			//TODO:  make it recognize changes in the first double member
			//print_r($_POST);
			$list = "";
			foreach($this->doubles as $tag => $value)
			{
				//echo "<BR> $value vs $_POST[$tag]<BR>";
				if(isset($_POST[$tag]))
				{

					if($_POST[$tag] != $this->doubles[$tag])
					{
						// echo ("$tag modified!<BR>\n");
						if ( $list == "" ) { $list = $tag; }
						else { $list .= ", $tag"; }
						$this->doubles[$tag] = $_POST[$tag];
					}
				}
			}
		
			foreach($this->ints as $tag => $value)
			{
				if(isset($_POST[$tag]))
				{
			
					if($_POST[$tag] != $this->ints[$tag])
					{
						// echo ("$tag modified!<BR>\n");
						if ( $list == "" ) { $list = $tag; }
						else { $list .= ", $tag"; }
						$this->ints[$tag] = $_POST[$tag];
					}
				}
			}
		
			if(isset($this->strings))
			foreach($this->strings as $tag => $value)
			{
				if(isset($_POST[$tag]))
				{
			
					if($_POST[$tag] != $this->strings[$tag])
					{
						// echo ("$tag modified!<BR>\n");
						if ( $list == "" ) { $list = $tag; }
						else { $list .= ", $tag"; }
						$this->strings[$tag] = $_POST[$tag];
					}
				}
			}
		
			if(isset($this->intArrays))
			foreach($this->intArrays as $tag => $value)
			{
				if(isset($_POST[$tag]))
				{
					$newValue = removeChar(" ", $_POST[$tag]);				
					$newValue = explode(",", $newValue);
					$this->intArrays[$tag] = $newValue;
				}
			}

			if(isset($this->stringArrays))
			foreach($this->stringArrays as $tag => $value)
			{		
				if(isset($_POST[$tag]))
				{
					$newValue = removeChar(" ", $_POST[$tag]);
					$newValue = explode(",", $newValue);
					$this->stringArrays[$tag] = $newValue;
				}
			}

			if(isset($this->doubleArrays))
			foreach($this->doubleArrays as $tag => $value)
			{
				if(isset($_POST[$tag]))
				{
					$newValue = removeChar(" ", $_POST[$tag]);				
					$newValue = explode(",", $newValue);
					$this->doubleArrays[$tag] = $newValue;
				}
			}

			if ( $list != "" ) { echo "<p><strong><font color=\"#990099\">The following data members were successfully updated: $list</font></strong><br>&nbsp;</p>"; }
			else { echo "<p><strong><font color=\"#990099\">No changes were made.</font></strong><br>&nbsp;</p>"; }

			$this->buildSQLStrings();
		}




		// Build an SQL string suitable for uploading an entire dataset to a blob in the database.

		function getSQLString()
		{
			$this->buildSQLStrings();
			$bigString = "$this->ID" . "&&&" .
			$this->intString . "&&&" .
			$this->doubleString	. "&&&" .
			$this->stringString . "&&&" .
			$this->intArrayString . "&&&" .
			$this->doubleArrayString . "&&&" .
			$this->stringArrayString;
			return $bigString;
		}
	}

?>