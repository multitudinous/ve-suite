<?php

	include_once("utility.php");

	class PPLink
	{
		var $vertices;		// elbows in this link
		var $vertexCount;
		var $fromModID;		// module ID this connects from
		var $toModID;		// module ID this connedts to
		var $fromPort;		// port this connects from
		var $toPort;		// port this connects from
		var $fromType;		// type of module this connects from
		var $toType;		// type of module this connects to
		var $ID;
	
		function PPLink()
		{
			static $i = 0;
			$this->ID = $i++;
			$this->clear();
		}
	
		function addVertex($x, $y)
		{
			$this->vertices[$this->vertexCount]['x'] = $x;
			$this->vertices[$this->vertexCount]['y'] = $y;	
			$this->vertexCount++;
		}
	
		function clear()
		{
			unset($this->vertices);
			$this->vertexCount = 0;
		}

		function draw(&$image, $color)
		{
			if($this->vertexCount < 2)
			{
				echo("Degenerate link -- cannot draw.<BR>\n");
				return;
			}

			for($i = 0; $i < $this->vertexCount - 1; $i++)
			{
				imageLine($image, 
					$this->vertices[$i]['x'], 
					$this->vertices[$i]['y'],
					$this->vertices[$i + 1]['x'], 
					$this->vertices[$i + 1]['y'], 
					$color);
			}
		
			$i = $this->vertexCount - 2;
			drawArrow($image,					
				$this->vertices[$i]['x'], 
				$this->vertices[$i]['y'],
				$this->vertices[$i + 1]['x'], 
				$this->vertices[$i + 1]['y'], 
				6, $color);
		}
	}

?>