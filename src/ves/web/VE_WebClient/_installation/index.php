<?php
	
	if ( ! isset($_SERVER['HTTPS'])) {
		header ("location: https://" . $_SERVER['SERVER_NAME'] . $_SERVER['PHP_SELF'] );
	} else {
	
	// Header information to control browser cache.
	
	header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
	header("Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT");
	header("Cache-Control: no-store, no-cache, must-revalidate");
	header("Cache-Control: post-check=0, pre-check=0", false);
	header("Pragma: no-cache");
	
	include_once("progress.php");
	
	$step = ".1";

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
			<td width="415" align="left" valign="middle"><h2>Welcome</h2></td>
			<td width="250" align="right" valign="middle"><?php progressBar($step, $total); ?></td>
			</tr>
		</table>

		<p>Welcome to the VE-Suite Data Viewer web installation. This web application will assist you in installing and setting up the VE-Suite Data Viewer. Before you begin, please make sure you have thoroughly reviewed the information found in the readme.txt file on your VE-Suite Data Viewer CD.<br>&nbsp;</p>
		
		<p><strong><u>System Requirements</u></strong></p>
		
		<p>The VE-Suite Data Viewer was developed for the following server specifications:</p>
		
			<ul>
			<li>Apache 2.0 with SSL compiled</li>
			<li>PHP 4.3.9 or greater</li>
			<li>MySQL 4.0.13 or greater</li>
			</ul>
			
		<p>Additionally, the file system on which this web application will be located must initially be mounted with write permissions to properly create the configuration file. After installation, you may close permissions to read-only.</p>
		
		<p>In order for this web application to function properly, we suggest that your server be upgraded to these specifications or beyond. <strong>For security purposes, SSL must be compiled on your server.<br>&nbsp;</strong></p>
		
		<p><strong><u>Browser Requirements</u></strong></p>
		
		<p>Because of our committment to open source technology, this web application was developed using the Mozilla Firefox browser. Although not a requirement, we suggest that Firefox 1.0.2 or greater be used when using this website. This web application <u>will</u> function properly on Mozilla and Internet Explorer browsers.</p>

			<ul>
			<li>If you have a pop-up blocker installed on your computer, or if a pop-up blocker feature is included in your browser, you must either disable it or put this site on the allowed list to use the chat and Virtual Viewer features.</li>
			<li>To use many of the available features, JavaScript must be enabled. JavaScript is enabled by default on most browsers; if you have previously disabled JavaScript, you will want to enable it to take advantage of all features.</li>	
			<li>SSL is used to create a secure connection between your computer and this web application. SSL must be enabled to help secure your data.</li>
			</ul>

		<p>This web application has also been designed for minimum screen resolution of 1024 by 768 pixels. To be able to view this site properly, we suggest you verify that your screen resolution is set at or above this setting.<br>&nbsp;</p>
	
		<p><strong><u>Support</u></strong></p>
		
		<p>During this installation, you will see several linked terms. These links will lead you to a <a href="faq.php" target="new">Frequently Asked Questions</a> page containing information about the link. Your installation will not be inturrupted.</p>
		
		<p>If you encounter issues that cannot be resolved by you or your system administrator, you may e-mail <a href="mailto:vesuite-support@iastate.edu">vesuite-support@iastate.edu</a>.<br>&nbsp;</p>
		
		<p><strong><u>Installation</u></strong></p>
		
		<p>Prior to beginning installation, please make sure that your system administrator has created or specified to you a database for use by the VE-Suite Data Viewer.</p>
		
		<p>Depending on your server configuration, this installation should be complete in 15 minutes or less, barring any unforseen errors.</p>
		
		<p>During the installation, this web application will primarily set up database tables specifically for use by the VE-Suite Data Viewer. Please make sure you have your MySQL database and web-side usernames and passwords available.</p>
	
		<p>&nbsp;</p>
		
		<form action="install_1.php" method="post">
		
		<p align="right"><input type="submit" name="begin_installation" value="Begin Installation  -->"></p>
		
		</form>
	
	</td>
	</tr>
</table>


</body>
</html>

<?php } ?>