<?php

	session_start();
	include_once("support/configuration.php");
	
	if ( ! isset($_SERVER["HTTPS"]) ) { header ("location: https://" . $_SERVER["SERVER_NAME"] . $_SERVER["PHP_SELF"] ); }
	else {
	
	if ( $_POST["login"] ) {
	
		function set_session( $username, $name, $email, $admin ) {
		
			$_SESSION["s_username"] 	= $username;
			$_SESSION["s_name"]			= $name;
			$_SESSION["s_email"]		= $email;
			$_SESSION["s_admin"]		= $admin;
			$_SESSION["s_verify"]		= session_id() . $_SERVER["REMOTE_ADDR"];
		
		}
		
		function last_login( $username ) {
		
			$query = "UPDATE ".$project_name."users SET last_login = '" . date("F d, Y, G:i A") . "' WHERE username = '$username'";
			$result = mysql_query($query);
		
		}
	
		$username = $_POST["username"];
		$password = $_POST["password"];
		
		if ( ! $username || ! ctype_alnum($username) ) { $error = 1; }
		elseif ( ! $password || ! ctype_alnum($username) ) { $error = 1; }
		else {
			
			$query = "SELECT * FROM ".$project_name."users WHERE username = '$username' AND password = '$password'";
			$result = mysql_query($query);
			$count = mysql_num_rows($result);
			
			if ( $count != 1 ) { $error = 1; }
			else {
			
				$result = mysql_fetch_assoc($result);
					
				if ( $result["last_login"] == "new_user" ) { 
					
					set_session( $result["username"], $result["name"], $result["email"], $result["admin"] );
				//	last_login( $username );	
					header ( "location: data_viewer.php?new_user" );
					
				} else { 
				
					set_session( $result["username"], $result["name"], $result["email"], $result["admin"] );
					last_login( $username );	
					header ( "location: data_viewer.php" );
				
				}
				
			}
		
		}
		
	}
	
?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<link href="style.css" rel="stylesheet" type="text/css">
	<title>VE-Suite Data Viewer</title>
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

		<h1>VE-Suite Data Viewer</h1>

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
	<td>

		&nbsp;<br>

		<h2>Login</h2>

	</td>
	</tr>
	<tr>
	<td></td>
	<td valign="top">

		<form action="index.php"  method="post">

		<table width="615" border="0" cellspacing="0" cellpadding="0">
			<tr>
			<td width="615" colspan="3">
						
				<table width="615" cellpadding="0" cellspacing="0" border="0"><tr>
				
				<?php
					
					if ( isset($error) || isset($session) || isset($logout) ) {
					
		
						if ( isset($logout) ) { $text = "<font color=\"#000066\">You have successfully logged out.</font>"; }
						elseif ( isset($error) ) { $text = "<font color=\"#FF0000\">You did not provide a valid username or password. Please try again.</font>"; }
						elseif ( isset($session) ) { $text = "<font color=\"#FF0000\">Your session has either expired or your session credentials are invalid.<br>Please login again.</font>"; }
						
						echo "<td height=\"16\" valign=\"top\"><strong>" . $text . "</strong></td>";
		
					} else { echo "<td height=\"16\" valign=\"top\">Please login to use the VE-Suite Website Administration web application.</td>"; }
					
				?>
				
				</tr></table>

			</td>
			</tr>
			<tr>
			<td colspan="3">&nbsp;<br>&nbsp;</td>
			</tr>
			<tr>
			<td width="75">Username:</td>
			<td width="220"><input name="username" type="text" size="30"></td>
			<td>&nbsp;</td>
			</tr>
			<tr>
			<td height="5" colspan="3"></td>
			<tr>
			<td>Password:</td>
			<td><input name="password" type="password" size="30"></td>
			<td align="left"><input type="submit" name="login" value="Login"></td>
			</tr>
			<tr>
			<td colspan="3">&nbsp;<p>&nbsp;<p><a href="reset_password.php">Forgot your password?</a><br><a href="browser_requirements.php">Browser Requirements</a></center></td>
			</tr>
		</table>

		</form>
	
	</td>
	</tr>
</table>


</body>
</html>

<?php } ?>