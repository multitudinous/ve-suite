<?php

	include_once("../support/configuration.php");
	
	if ( $_POST["update"] ) {
	
		if ( $_POST["x"] == "0" ) { $x = "0"; } else { $x = $_POST["x"]; }
		if ( $_POST["y"] == "0" ) { $y = "0"; } else { $y = $_POST["y"]; }
		if ( $_POST["z"] == "0" ) { $z = "0"; } else { $z = $_POST["z"]; }
		
		
		// Find r
		
		$r = sqrt(
			(pow($x, 2)) +
			(pow($y, 2)) +
			(pow($z, 2))
			);
		
		
		// Find theta
		
			// Because of a divide-by-zero error in PHP, remove chances for a divide by zero and reassign
			// value to theta to avoid the error.
		
		if ( ( $x == "0" ) || ( $x == 0 ) || ( $x == "" ) ) { $theta = "0"; } 
		else { $theta = atan(($y/$x)); }
		
		
		// Find phi
		
			// Because of a divide-by-zero error in PHP, remove chances for a divide by zero and reassign
			// value to phi to avoid the error.
			
		if ( ( $r == "0" ) || ( $r == 0 ) || ( $r == "" ) ) { $phi = "0"; } 
		else { $phi = acos(($z/$r)); }	
		
		// Convert phi and theta to degrees. (R does not get converted to degress)
		
		$phi = rad2deg($phi);
		$theta = rad2deg($theta);
		
		$query = "UPDATE ".$project_name."vv SET x = '$x', y = '$y', z = '$z', r = '$r', theta = '$theta', phi = '$phi' WHERE id = '1'";
		$result = mysql_query($query);

		$page = "controls";
	
	}

	elseif ( $_POST["r"] || $_POST["theta"] || $_POST["phi"] || $_POST["r"] == "0" || $_POST["theta"] == "0" || $_POST["phi"] == "0" ) {
	
		
		// R value checks.
		
		if ( $_POST["r"] ) { $r = $_POST["r"]; } else { $r = $_POST["r_old"]; }
		
			if ( $_POST["r"] == "0" || $_POST["r"] == "-5" ) 	{ $r = "0"; }
			elseif ( $_POST["r"] < 0 ) 							{ $r = "0"; }


		// Theta value checks.

		if ( $_POST["theta"] ) { $theta = $_POST["theta"]; } else { $theta = $_POST["theta_old"]; }
		
			if ( $_POST["theta"] == "0" || $_POST["theta"] == "-5" ) 	{ $theta = "0"; }
			elseif ( $_POST["theta"] == "360" ) 						{ $theta = "0"; /* $theta = "360"; */ }


		// Phi value checks.

		if ( $_POST["phi"] ) { $phi = $_POST["phi"]; } else { $phi = $_POST["phi_old"]; }
		
			if ( $_POST["phi"] == "0" ) 		{ $phi = "0"; }
			elseif ( $_POST["phi"] == "185" ) 	{ $phi = "0"; }
			elseif ( $_POST["phi"] == "-5" ) 	{ $phi = "180"; }
			elseif ( $_POST["phi"] < 0 ) 		{ $phi = "0"; }


		// Calculations.

		$x = ( $r * (cos(deg2rad($theta))) * (sin(deg2rad($phi))) );
		$y = ( $r * (sin(deg2rad($theta))) * (sin(deg2rad($phi))) );
		$z = ( $r * (cos(deg2rad($phi))) );

		$query = "UPDATE ".$project_name."vv SET x = '$x', y = '$y', z = '$z', r = '$r', theta = '$theta', phi = '$phi' WHERE id = '1'";
		$result = mysql_query($query);

		$page = "controls";

	}
	
	elseif ( $_POST["reset"] ) {
	
		
		// If reset, return x, y, z, r, theta, and phi to zero.

		$query = "UPDATE ".$project_name."vv SET x = '0', y = '0', z = '0', r = '0', theta = '0', phi = '0' WHERE id = '1'";
		$result = mysql_query($query);

		$page = "controls";
	
	}

?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">

<?php if ( $page == "viewer" ) {?>

<!-- // VIEWER // -->

<html>

<head>
	<!-- <meta http-equiv="refresh" Content="5; vv.php?page=viewer"> -->
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<link href="style.css" rel="stylesheet" type="text/css">
	<title></title>
</head>

<body>

<center><img src="vv.jpg" width="600" height="600" border="1"></center>

</body>
</html>







<?php } elseif ( $page == "controls" ) { ?>

<!-- // CONTROLS // -->

<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<link href="style.css" rel="stylesheet" type="text/css">
	<title></title>
</head>

<body>

<table width="100%" cellpadding="0" cellspacing="0" border="0">
	<tr>
	<td width="50%" align="center" valign="top">
	
		<table width="190" cellpadding="0" cellspacing="0" border="0">
			
			<form action="vv.php" method="post">
		
			<tr>
			<td colspan="2" align="center"><strong>Manual Input</strong></td>
			</tr>
			<tr>
			<td colspan="2" align="center"><hr width="190" size="1" noshade color="#000000" style="margin: 1px;"></td>
			</tr>
			<tr>
			<td height="5" colspan="2"></td>
			</tr>
			<tr>
			<td colspan="2" align="center">
			
				<?php
				
					$query = "SELECT * FROM ".$project_name."vv WHERE id = '1'";
					$result = mysql_query($query);
					$result = mysql_fetch_assoc($result);
					
					$x = $result["x"];
					$y = $result["y"];
					$z = $result["z"];
					
					$r = $result["r"];
					$theta = $result["theta"];
					$phi = $result["phi"];
				
				?>
			
				<table width="165" cellpadding="0" cellspacing="0" border="0">
					<tr>
					<td width="15">X:</td>
					<td width="40"><input type="text" name="x" size="3" maxlength="3" value="<?php echo $x; ?>" style="font-family: Courier New;"></td>
					<td width="15">Y:</td>
					<td width="40"><input type="text" name="y" size="3" maxlength="3" value="<?php echo $y; ?>" style="font-family: Courier New;"></td>
					<td width="15">Z:</td>
					<td width="40"><input type="text" name="z" size="3" maxlength="3" value="<?php echo $z; ?>" style="font-family: Courier New;"></td>
					</tr>
				</table>
			
			</td>
			</tr>
			<tr>
			<td height="5" colspan="2"></td>
			</tr>
			<tr>
			<td colspan="2"><hr width="190" size="1" noshade color="#000000" style="margin: 1px;"></td>
			</tr>
			<tr>
			<td height="3" colspan="2"></td>
			</tr>
			<tr>
			<td align="left"><input type="submit" name="reset" value="Reset"></td>
			<td align="right"><input type="submit" name="update" value="Update"></td>
			</tr>
			
			</form>
			
		</table>
	
	</td>
	<td width="50%" align="center" valign="top">
	
		<table width="190" cellpadding="0" cellspacing="0" border="0">
			
			<form action="vv.php" method="post">
			<input type="hidden" name="r_old" value="<?php echo $r; ?>">
			<input type="hidden" name="theta_old" value="<?php echo $theta; ?>">
			<input type="hidden" name="phi_old" value="<?php echo $phi; ?>">
			
			<tr>
			<td align="center"><strong>Navigation</strong></td>
			</tr>
			<tr>
			<td align="center"><hr width="190" size="1" noshade color="#000000" style="margin: 1px;"></td>
			</tr>
			<tr>
			<td align="center" style="font-family: Verdana, Tahoma, sans-serif; font-size: 10px;">
			
				<table width="100%" cellpadding="0" cellspacing="0" border="0">
					<tr>
					<td align="left"><?php echo "R: $r"; ?></td>
					<td align="center"><?php echo "Theta: $theta"; ?></td>
					<td align="right"><?php echo "Phi: $phi"; ?></td>
					</tr>
				</table>
				
			</tr>
			<tr>
			<td align="center"><hr width="190" size="1" noshade color="#000000" style="margin: 1px;"></td>
			</tr>
			<tr>
			<td align="center">
			
				<table cellpadding="2" cellspacing="0" border="0">
					<tr>
					<td width="19" height="19"><input type="image" name="r" value="<?php echo ($r+5); ?>" src="button_plus.gif" width="19" height="19" border="0"></td>
					<td width="47" height="19"></td>
					<td width="19" height="19"></td>
					<td width="19" height="19"><input type="image" name="phi" value="<?php echo ($phi-5); ?>" src="arrow_up.gif" width="19" height="19" border="0"></td>
					<td width="19" height="19"></td>
					</tr>
					<tr>
					<td width="19" height="19"><img src="button_slide.gif" width="19" height="19" border="0"></td>
					<td width="47" height="19"></td>
					<td width="19" height="19"><input type="image" name="theta" value="<?php echo ($theta-5); ?>" src="arrow_left.gif" width="19" height="19" border="0"></td>
					<td width="19" height="19"><img src="arrow_center.gif" width="19" height="19" border="0"></td>
					<td width="19" height="19"><input type="image" name="theta" value="<?php echo ($theta+5); ?>" src="arrow_right.gif" width="19" height="19" border="0"></td>
					</tr>
					<tr>
					<td width="19" height="19"><input type="image" name="r" value="<?php echo ($r-5); ?>" src="button_minus.gif" width="19" height="19" border="0"></td>
					<td width="47" height="19"></td>
					<td width="19" height="19"></td>
					<td width="19" height="19"><input type="image" name="phi" value="<?php echo ($phi+5); ?>" src="arrow_down.gif" width="19" height="19" border="0"></td>
					<td width="19" height="19"></td>
					</tr>
				</table>
			
			</td>
			</tr>
			
			</form>
			
		</table>
	
	</td>
	</tr>
</table>

</body>
</html>









<?php } else { ?>

<!-- // FRAMESET // -->

<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<title>Virtual Viewer</title>
</head>

<frameset rows="620,125" cols="*" frameborder="true" border="1" framespacing="0">
	<frame src="vv.php?page=viewer" name="viewer" scrolling="auto" noresize>
	<frame src="vv.php?page=controls" name="controls" scrolling="auto" noresize>
	</frameset>
</frameset>

<noframes>
	<body>
	</body>
</noframes>

<?php } ?>