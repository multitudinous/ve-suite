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

<a name="top"></a>

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

		<h2>Frequently Asked Questions</h2>
		
		<p>
		
		<a href="#filesystem" class="faq">Why must this file system be mounted with write permissions?</a><br>
		<a href="#websideusername" class="faq">What is a Web-side Username?</a><br>
		<a href="#websidepassword" class="faq">What is a web-side Password?</a><br>
		<a href="#mysqlserver" class="faq">What is the MySQL server?</a><br>
		<a href="#mysqldatabase" class="faq">What is the MySQL database?</a><br>
		<a href="#projectname" class="faq">What is the Project Name?</a><br>
		<a href="#dbusername" class="faq">What is the Database Username?</a><br>
		<a href="#dbpassword" class="faq">What is the Database Password?</a><br>
		<a href="#accounttypes" class="faq">What are the different user account types?</a>
		
		</p>
		
		<p>&nbsp;</p>

		<p>
		<strong><a name="filesystem" class="anchor">Why must this file system be mounted with write permissions?</a></strong><br>
		Initially, the file system on which this web application is located must be mounted with write permissions to create the configuration file. After installation, you may close these permissions.
		</p>
		
		<p><a href="#top" class="faq">Return to Top</a></p>
		<p>&nbsp;</p>

		<p>
		<strong><a name="websideusername" class="anchor">What is a Web-side Username?</a></strong><br>
		For security reasons, the web-side username is different than the username you would use to create databases, create tables, etc. The web-side username only has permissions to add, modify, and delete rows from tables within your database. Although not required, we strongly suggest that you create a web-side username if you have not done so already. If you are unable to create one, you may use your database username instead.
		</p>
		
		<p><a href="#top" class="faq">Return to Top</a></p>
		<p>&nbsp;</p>
		
		<p>
		<strong><a name="websidepassword" class="anchor">What is a Web-side Password?</a></strong><br>
		This is the password associated with your <a href="#websideusername" class="faq">Web-side Username</a>.
		</p>
		
		<p><a href="#top" class="faq">Return to Top</a></p>
		<p>&nbsp;</p>
		
		<p>
		<strong><a name="mysqlserver" class="anchor">What is the MySQL Server?</a></strong><br>
		This is the server address on which your MySQL database resides. Example: mysql.mywebsite.com
		</p>
		
		<p><a href="#top" class="faq">Return to Top</a></p>
		<p>&nbsp;</p>
		
		<p>
		<strong><a name="mysqldatabase" class="anchor">What is the MySQL Database?</a></strong><br>
		This is the name of the database that you want the VE-Suite Data Viewer to use. You must create the database prior to installation.
		</p>
		
		<p><a href="#top" class="faq">Return to Top</a></p>
		<p>&nbsp;</p>
		
		<p>
		<strong><a name="projectname" class="anchor">What is the Project Name?</a></strong><br>
		Defining a project name for the VE-Suite Data Viewer is suggested. Installation will prepend the provided project name to all tables associated with the data viewer in the database, making these tables easier to identify and less likely to be accidentally deleted.
		</p>
		
		<p><a href="#top" class="faq">Return to Top</a></p>
		<p>&nbsp;</p>
		
		<p>
		<strong><a name="dbusername" class="anchor">What is the Database Username?</a></strong><br>
		This is the username to the <a href="#mysqldatabase" class="faq">MySQL database</a> that you defined earlier. This username has the permissions to create, modify, and delete tables.
		</p>
		
		<p><a href="#top" class="faq">Return to Top</a></p>
		<p>&nbsp;</p>
		
		<p>
		<strong><a name="dbpassword" class="anchor">What is the Database Password?</a></strong><br>
		This is the password associated with your <a href="#dbusername" class="faq">database username</a>.
		</p>
		
		<p><a href="#top" class="faq">Return to Top</a></p>
		<p>&nbsp;</p>
		
		<p>
		<strong><a name="accounttypes" class="anchor">What are the different user account types?</a></strong><br>
		Administrative user accounts have the ability to add / delete users, 
		</p>
		
		<p><a href="#top" class="faq">Return to Top</a></p>
		<p>&nbsp;</p>
	
	</td>
	</tr>
</table>


</body>
</html>

<?php } ?>