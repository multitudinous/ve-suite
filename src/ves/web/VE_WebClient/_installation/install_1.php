<?php
	
	if ( ! isset($_SERVER['HTTPS'])) {
		header ("location: index.php" );
	} else {

	if ( isset($_POST["step_1"]) ) {
	
		$web_side_username 	= $_POST["web_side_username"];
		$web_side_password 	= $_POST["web_side_password"];
		$server 			= $_POST["server"];
		$database 			= $_POST["database"];
		$project_name 		= $_POST["project_name"] . "_";
		$database_username 	= $_POST["database_username"];
		$database_password	= $_POST["database_password"];
		
		$error = "";
		
		if ( ! $web_side_username ) { $error .= "You must enter a web-side username to continue.<br>"; }
		if ( ! $web_side_password ) { $error .= "You must enter a web-side password to continue.<br>"; }
		if ( ! $server ) { $error .= "You must specify a MySQL server.<br>"; }
		if ( ! $database ) { $error .= "You must specify a MySQL database.<br>"; }
		if ( ! $database_username ) { $error .= "You must enter a database username to continue.<br>"; }
		if ( ! $database_password ) { $error .= "You must enter a database password to continue.<br>"; }
		
		if ( $error == "" ) {			
			
			if ( ! @mysql_connect($server, $database_username, $database_password) ) { $error = "Unable to connect to the specified database server. Check your MySQL database server, username, and password.<br>&nbsp;<br>This was returned by the database server:<br>".mysql_error(); }
			else {
			
			if ( ! @mysql_select_db($database) ) { $error = "Unable to connect to the specified database. Check your information.<br>&nbsp;<br>This was returned by the database server:<br>".mysql_error(); }
			else {

				// Check to see if tables already exist and create tables.

				$query = "SELECT 1 FROM ".$project_name."chat_messages LIMIT 0";
				$result = mysql_query($query);
				if ( $result ) { $error .= "Could not create ".$project_name."chat_messages. This table already exists.<br>"; }
				else {
				
					$query = "CREATE TABLE ".$project_name."chat_messages ( id INT NOT NULL AUTO_INCREMENT , user_id INT NOT NULL , posted TIMESTAMP , message VARCHAR( 255 ) NOT NULL , PRIMARY KEY ( id ));";
					$result = mysql_query($query); // or die("Could not create ".$project_name."chat_messages table.");
								
					if ( ! $result ) { $error .= "Could not create ".$project_name."chat_messages table.<br>This was returned by the database server: ".mysql_error()."<br><br>"; }
				
				}

				$query = "SELECT 1 FROM ".$project_name."chat_users LIMIT 0";
				$result = mysql_query($query);
				if ( $result ) { $error .=  "Could not create ".$project_name."chat_users. This table already exists.<br>"; }
				else {
				
					$query = "CREATE TABLE ".$project_name."chat_users ( id INT NOT NULL AUTO_INCREMENT , username VARCHAR ( 50 ) , last_update TIMESTAMP NOT NULL , PRIMARY KEY ( id ));";
					$result = mysql_query($query); // or die("Could not create ".$project_name."chat_users table.");
			
					if ( ! $result ) { $error .= "Could not create ".$project_name."chat_users table.<br>This was returned by the database server: ".mysql_error()."<br><br>"; }
				
				}

				$query = "SELECT 1 FROM ".$project_name."data LIMIT 0";
				$result = mysql_query($query);
				if ( $result ) { $error .=  "Could not create ".$project_name."data. This table already exists.<br>"; }
				else {
				
					$query = "CREATE TABLE ".$project_name."data ( id INT ( 0 ) NOT NULL , user VARCHAR ( 50 ) , data BLOB , notes BLOB , name VARCHAR ( 50 ) , PRIMARY KEY ( id ));";
					$result = mysql_query($query); // or die("Could not create ".$project_name."data table.");
			
					if ( ! $result ) { $error .= "Could not create ".$project_name."data table.<br>This was returned by the database server: ".mysql_error()."<br><br>"; }
			
					include_once("data.php");
				
				}

				$query = "SELECT 1 FROM ".$project_name."files LIMIT 0";
				$result = mysql_query($query);
				if ( $result ) { $error .=  "Could not create ".$project_name."files. This table already exists.<br>"; }
				else {
				
					$query = "CREATE TABLE ".$project_name."files ( id INT NOT NULL AUTO_INCREMENT , filename VARCHAR ( 255 ) , components VARCHAR ( 255 ) , data BLOB , mimeType VARCHAR ( 255 ) , user VARCHAR ( 255 ) , username VARCHAR ( 255 ) , comments TEXT , PRIMARY KEY ( id ));";
					$result = mysql_query($query); // or die("Could not create ".$project_name."files table.");
			
					if ( ! $result ) { $error .= "Could not create ".$project_name."files table.<br>This was returned by the database server: ".mysql_error()."<br><br>"; }
				
				}

				$query = "SELECT 1 FROM ".$project_name."users LIMIT 0";
				$result = mysql_query($query);
				if ( $result ) { $error .=  "Could not create ".$project_name."users. This table already exists.<br>"; }
				else {
				
					$query = "CREATE TABLE ".$project_name."users ( id INT NOT NULL AUTO_INCREMENT , username VARCHAR ( 255 ) NOT NULL , password VARCHAR ( 255 ) NOT NULL, email VARCHAR ( 255 ) NOT NULL, name VARCHAR ( 255 ) NOT NULL, admin VARCHAR ( 255 ) NOT NULL, last_login VARCHAR ( 255 ) NOT NULL , PRIMARY KEY ( id ));";
					$result = mysql_query($query); // or die("Could not create ".$project_name."users table.");
			
					if ( ! $result ) { $error .= "Could not create ".$project_name."users table.<br>This was returned by the database server: ".mysql_error()."<br><br>"; }
				
				}

				$query = "SELECT 1 FROM ".$project_name."vv LIMIT 0";
				$result = mysql_query($query);
				if ( $result ) { $error .=  "Could not create ".$project_name."vv. This table already exists.<br>"; }
				else {
				
					$query = "CREATE TABLE ".$project_name."vv ( id INT NOT NULL AUTO_INCREMENT , x INT ( 0 ) NOT NULL , y INT ( 0 ) NOT NULL , z INT ( 0 ) NOT NULL , r INT ( 0 ) NOT NULL , theta INT ( 0 ) NOT NULL , phi INT ( 0 ) NOT NULL , PRIMARY KEY ( id ));";
					$result = mysql_query($query); // or die("Could not create ".$project_name."vv table.");
			
					if ( ! $result ) { $error .= "Could not create ".$project_name."vv table.<br>This was returned by the database server: ".mysql_error()."<br><br>"; }
					else {
					
						$query = "INSERT INTO ".$project_name."vv SET x = '0', y = '0', z = '0', r = '0', theta = '0', phi = '0'";
						$result = mysql_query($query);
						
						if ( ! $result ) { $error .= "Could not insert row into ".$project_name."vv. This was returned by the database server: ".mysql_error()."<br><br>"; }
					
					}
					
				}
			
				if ( ! $error > 0 ) {
			
					// When given errors, the user has to correct their input information. Writing the config file needs to go at
					// the very end to avoid multiple sets of configuration data being written to the same file -- this can cause
					// major problems.
		
					$url = $_SERVER['PHP_SELF'];
					$url = str_replace( "_installation/install_1.php", "", $url );
					$url = "http://" . $_SERVER['SERVER_NAME'] . $url;
				
					$data = "<?php\n\n";
					$data .= "\$server = \"".$server."\";\n";
					$data .= "\$database = \"".$database."\";\n";
					$data .= "\$uname = \"".$web_side_username."\";\n";
					$data .= "\$password = \"".$web_side_password."\";\n";
					$data .= "\$project_name = \"".$project_name."\";\n\n";
					$data .= "\$url = \"".$url."\";\n\n";
					$data .= "@mysql_pconnect(\$server, \$uname, \$password) or die (\"Could not connect to database server. Please try again or contact your system administrator.\");\n@mysql_select_db(\$database) or die(\"Could not select database. Please try again or contact your system administrator.\");\n\n";
					$data .= "?>";
					$file = "../support/configuration.php";
					if ( ! $file_handle = fopen($file, "w") ) { $error .= "Cannot open configuration file. Please be sure you have run the configuration shell script.<br>"; }
					if ( ! fwrite($file_handle, $data) ) { $error .= "Cannot write to file. Please be sure you have run the configuration shell script."; }
					fclose($file_handle);
					
					unset( $web_side_username, $username, $web_side_password, $password, $server, $database, $project_name, $database_username, $database_password );
					
					if ( ! $error ) { header("location: install_2.php"); }
					
				} 
				
				/* else {
				
					$query = "DROP TABLE '".$project_name."chat_messages', '".$project_name."chat_users', '".$project_name."data', '".$project_name."files', '".$project_name."users', '".$project_name."vv'";
					$result = mysql_query($query);
				
				}
				
				*/
	
			} }
			
		}
		
		// Need to reset $project_name if there are errors ... "_" has been appended to it when checking and inserting tables.
		
		$project_name = $_POST["project_name"];
	
	}

	// Header information to control browser cache.
	
	header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
	header("Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT");
	header("Cache-Control: no-store, no-cache, must-revalidate");
	header("Cache-Control: post-check=0, pre-check=0", false);
	header("Pragma: no-cache");
		
	include_once("progress.php");

	$step = ".2";

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
		
		<form action="install_1.php" method="post">

		&nbsp;<br>

		<table width="615" cellpadding="0" cellspacing="0" border="0">
			<tr>
			<td width="415" align="left" valign="middle"><h2>Step 1 of <?php echo $total; ?></h2></td>
			<td width="250" align="right" valign="middle"><?php progressBar($step, $total); ?></td>
			</tr>
		</table>
		
		<?php if ( $error ) { ?>
					
		<table width="615" cellpadding="7" cellspacing="1" border="0" bgcolor="#CC0000">
			<tr>
			<td bgcolor="#FFFFFF">
			
				<font color="#CC0000">
				<strong>
				
				The following errors occured while attempting to set up tables within the database...
				
				<blockquote><?php echo $error; ?></blockquote>
				
				Please try again or contact your system administrator. For security purposes, you must enter your web-side and database passwords again.
				
				</strong>
				</font>
			
			</td>
			</tr>
		</table>
		
		<p>&nbsp;</p>
		
		<?php } ?>

		<p><strong><u>MySQL Information</u></strong></p>
		
		<p>The following information is needed to properly connect to the database from the web application.</p>
		
		<p>Your web-side username and password should be different than your database username and password, as this prevents unauthorized users from potentially altering table data. If you do not know the web-side username and password for this database, please check with your system administrator to make sure one is available.<br>&nbsp;</p>
		
		<table width="615" cellpadding="0" cellspacing="0" border="0">
			<tr>
			<td width="25"></td>
			<td width="150"><a href="faq.php#websideusername" class="faq" target="new">Web-side Username</a>:</td>
			<td width="440"><input type="text" name="web_side_username" size="60" style="width: 440px;" value="<?php echo $web_side_username; ?>"></td>
			</tr>
			<tr>
			<td height="10" colspan="3"></td>
			</tr>
			<tr>
			<td width="25"></td>
			<td width="150"><a href="faq.php#websidepassword" class="faq" target="new">Web-side Password</a>:</td>
			<td width="440"><input type="password" name="web_side_password" size="60" style="width: 440px;"></td>
			</tr>
			<tr>
			<td height="30" colspan="3"></td>
			</tr>
			<tr>
			<td width="25"></td>
			<td width="150"><a href="faq.php#mysqlserver" class="faq" target="new">MySQL Server</a>:</td>
			<td width="440"><input type="text" name="server" size="60" style="width: 440px;" value="<?php echo $server; ?>"></td>
			</tr>
			<tr>
			<td height="10" colspan="3"></td>
			</tr>
			<tr>
			<td width="25"></td>
			<td width="150"><a href="faq.php#mysqldatabase" class="faq" target="new">MySQL Database</a>:</td>
			<td width="440"><input type="text" name="database" size="60" style="width: 440px;" value="<?php echo $database; ?>"></td>
			</tr>
		</table>
		
		<p>&nbsp;</p>

		<p><strong><u>Data Viewer Project Name</u> (optional)</strong></p>
		
		<p>If you are sharing this database with other users or projects, we suggest specifying a project name. By doing so, the project name will be prepended to all table names that belong to the VE-Suite Data Viewer in the database, making VE-Suite Data Viewer tables easy to identify and less likely to be mistakenly altered or deleted by other users.</p>
		
		<p>This project name must be no longer than five characters, lowercase, and not include spaces or punctuation. We suggest using "dv" -- which stands for "Data Viewer"<br>&nbsp;</p>
		
		<table width="615" cellpadding="0" cellspacing="0" border="0">
			<tr>
			<td width="25"></td>
			<td width="150"><a href="faq.php#projectname" class="faq" target="new">Project Name</a>:</td>
			<td width="440"><input type="text" name="project_name" size="60" maxlength="5" style="width: 440px;" value="<?php echo $project_name; ?>"></td>
			</tr>
		</table>
		
		<p>&nbsp;</p>

		<p><strong><u>MySQL Database Username and Password</u></strong></p>
		
		<p>This is the username and password to the database which has the ability to create, modify, and delete tables. The database username and password should be different than your web-side username and password.</p>
		
		<p>This information will be used only during installation and will be deleted once installation has been completed.<br>&nbsp;</p>

		<table width="615" cellpadding="0" cellspacing="0" border="0">
			<tr>
			<td width="25"></td>
			<td width="150"><a href="faq.php#dbusername" class="faq" target="new">Database Username</a>:</td>
			<td width="440"><input type="text" name="database_username" size="60" style="width: 440px;" value="<?php echo $database_username; ?>"></td>
			</tr>
			<tr>
			<td height="10" colspan="3"></td>
			</tr>
			<tr>
			<td width="25"></td>
			<td width="150"><a href="faq.php#dbpassword" class="faq" target="new">Database Password</a>:</td>
			<td width="440"><input type="password" name="database_password" size="60" style="width: 440px;"></td>
			</tr>
		</table>	
		
		<p>&nbsp;</p>
		
		<p>When you press "Continue Installation," the required tables and their associated properties will be created.</p>
		
		<p>&nbsp;</p>
		
		<p align="right"><input type="submit" name="step_1" value="Continue Installation  -->"></p>
		
		</form>
		
	</td>
	</tr>
</table>


</body>
</html>

<?php } ?>