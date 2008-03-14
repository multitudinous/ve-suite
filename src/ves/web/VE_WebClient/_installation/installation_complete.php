<?php
	
	if ( ! isset($_SERVER['HTTPS'])) {
		header ("location: index.php" );
	} else {

	// Header information to control browser cache.
	
	header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
	header("Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT");
	header("Cache-Control: no-store, no-cache, must-revalidate");
	header("Cache-Control: post-check=0, pre-check=0", false);
	header("Pragma: no-cache");
	
	include_once("progress.php");
	include_once("../support/configuration.php");
	
	$step = "4";

?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<META name="expires" content="Tue, 01 Jun 1999 19:58:02 GMT">
	<link href="style.css" rel="stylesheet" type="text/css">
	<title>VE-Suite Data Viewer Web Installation</title>
</head>

<body>

<table width="100%" cellpadding="0" cellspacing="0" border="0">
	<tr>
	<td width="35" height="25">&nbsp;</td>
	<td height="25">&nbsp;</td>
	<td height="25">&nbsp;</td>
	</tr>
	<tr>
	<td height="1" colspan="3" bgcolor="#000000"></td>
	</tr>
	<tr>
	<td height="80"></td>
	<td height="80" colspan="2" valign="middle">

		<h1>VE-Suite Data Viewer Web Installation</h1>

	</td>
	</tr>
	<tr>
	<td height="1" colspan="3" bgcolor="#000000"></td>
	</tr>
	<tr>
	<td height="20" colspan="3">&nbsp;</td>
	</tr>
	<tr>
	<td height="1" colspan="3" bgcolor="#000000"></td>
	</tr>
</table>

<table width="650" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td width="35">&nbsp;</td>
	<td valign="top">

		&nbsp;<br>

		<table width="615" cellpadding="0" cellspacing="0" border="0">
			<tr>
			<td width="415" align="left" valign="middle"><h2>Installation Complete</h2></td>
			<td width="250" align="right" valign="middle"><?php progressBar($step, $total); ?></td>
			</tr>
		</table>

		<p>Congratulations, you have successfully completed installation of the VE-Suite Data Viewer. A default power plant diagram with default components and default attributes has been placed in the database for you.</p>
		
		<p>The directory "_installation" may now be deleted.</p>
		
		<p>&nbsp;</p>
		
		<p>&nbsp;</p>
		
		<p>&nbsp;</p>
		
		<p>&nbsp;</p>
		
		<p>&nbsp;</p>
		
		<p>&nbsp;</p>
		
		<p>&nbsp;</p>
		
		<form action="../" method="post">
		
		<p align="right"><input type="submit" name="end_installation" value="Go to Remote Data Viewer  -->"></p>
		
		</form>
		
	</td>
	</tr>
</table>


</body>
</html>

<?php } ?>