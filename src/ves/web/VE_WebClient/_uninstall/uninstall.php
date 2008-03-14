<?php

	$post_username 	= $_POST['username'];
	$post_password 	= $_POST['password'];
	$database_username 	= $_POST['database_username'];
	$database_password 	= $_POST['database_password'];
	
	if ( $_POST["uninstall"] )
	{

		include_once("../support/configuration.php");
		

		// Pull information out of the database.

		$query = "SELECT * FROM ".$project_name."users WHERE username = '$post_username'";
		$result = mysql_query($query);
		$login = mysql_fetch_array($result, MYSQL_ASSOC);
		$login_username = $login['username'];
		$login_password = $login['password'];


		// Various error checking.

		if ( $post_username = 0 ) { $id = "3"; }
		elseif ( $post_login = 0 ) { $id = "3"; }
		elseif ( $login_username == "" ) { $id = "1"; }
		elseif ( $login_password != $post_password ) { $id = "2"; }

		elseif ( $login_password == $post_password && $login_username == $post_username )
		{	
		
			mysql_close();
		
			if ( ! @mysql_connect($server, $database_username, $database_password) ) { $error = "Unable to connect to the specified database server. Check your MySQL database server, username, and password.<br>&nbsp;<br>This was returned by the database server:<br>".mysql_error(); }
			else {
			
			if ( ! @mysql_select_db($database) ) { $error = "Unable to connect to the specified database. Check your information.<br>&nbsp;<br>This was returned by the database server:<br>".mysql_error(); }
			else {
			
				mysql_query("DROP TABLE ".$project_name."chat_messages");
				mysql_query("DROP TABLE ".$project_name."chat_users");
				mysql_query("DROP TABLE ".$project_name."data");
				mysql_query("DROP TABLE ".$project_name."files");
				mysql_query("DROP TABLE ".$project_name."users");
				mysql_query("DROP TABLE ".$project_name."vv");
			
				header ( "location: uninstall_complete.php" );
			
			} }

		}

	}
?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<link href="../style.css" rel="stylesheet" type="text/css">
	<title>VE-Suite Data Viewer</title>
	
	<script language="javascript">
	
		function confirmLink(theLink)
		{
		    if (confirmMsg == '' || typeof(window.opera) != 'undefined') { return true; }
			var is_confirmed = confirm(confirmMsg);
		    if (is_confirmed) { theLink.href; }
			return is_confirmed;
		}
		
		var confirmMsg  = 'Are you sure you want to uninstall the VE-Suite Remote Data Viewer?';

	</script>
</head>
<body>









<table width="650" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>

	<tr>
	<td>

		<p><strong><u>Uninstall VE-Suite Data Viewer</u></strong></p>
		
		<p>To continue, you must enter your VE-Suite Data Viewer administrator username and password and your <u>MySQL database username and password</u> to verify that you want to <strong>uninstall the VE-Suite Data Viewer.</strong></p>
		
		<p>&nbsp;</p>
		
		<?php
					
			if ( $id ) { echo "<p><strong>"; }
					
			if ( $id == "1" ) { ?><font color="#CC0000">Error: Invalid username or password.</font><? }
			elseif ( $id == "2" ) { ?><font color="#CC0000">Error: Invalid username or password.</font><? }
			elseif ( $id == "3" ) { ?><font color="#CC0000">Error: Invalid login.</strong><? }
			
			if ( $id ) { echo "</strong></p><p>&nbsp;</p>"; }
					
			if ( $error ) { echo "<p><strong>".$error." </strong></p><p>&nbsp;</p><p>&nbsp;</p>"; }
				 
		?>
		
		<form action="uninstall.php" method="post">
		
		<table border="0" cellspacing="0" cellpadding="0">
			<tr>
			<td width="50"></td>
			<td width="150">Data Viewer Username:</td>
			<td><input name="username" type="text" size="30"></td>
			</tr>
			<tr>
			<td height="5" colspan="3"></td>
			<tr>
			<td width="50"></td>
			<td>Data Viewer Password:</td>
			<td><input name="password" type="password" size="30"></td>
			</tr>
			<tr>
			<td colspan="3">&nbsp;</td>
			</tr>
			<tr>
			<td width="50"></td>
			<td width="75">MySQL Username:</td>
			<td><input name="database_username" type="text" size="30"></td>
			</tr>
			<tr>
			<td height="5" colspan="3"></td>
			<tr>
			<td width="50"></td>
			<td>MySQL Password:</td>
			<td><input name="database_password" type="password" size="30"></td>
			</tr>
		</table>
		
		<p>&nbsp;</p>
		
		<p>&nbsp;</p>
		
		<p align="right"><input type="submit" name="uninstall" onclick="return confirmLink( this )" value="Uninstall VE-Suite Data Viewer"></p>
		
		</form>

	</td>
	</tr>
</table>








</body>
</html>