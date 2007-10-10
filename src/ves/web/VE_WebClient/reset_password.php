<?php

	// Header information to control browser cache.
	header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
	header("Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT");
	header("Cache-Control: no-store, no-cache, must-revalidate");
	header("Cache-Control: post-check=0, pre-check=0", false);
	header("Pragma: no-cache");

	include_once("support/configuration.php");

	if ( $_POST['reset_password'] ) {

		$username = $_POST['username'];

		// Random Password Generator
		$allow = "abcdefghijkmnpqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
		srand((double)microtime()*1000000);
		for( $i=0; $i<8; $i++ ) { $password .= $allow[rand()%strlen($allow)]; }

		// Commit the new password to the database.
		$query = "UPDATE ".$project_name."users SET password = '$password' WHERE username = '$username'";
		$result = mysql_query($query);

		// Retrieve the user's e-mail address.
		$query = "SELECT * FROM ".$project_name."users WHERE username = '$username'";
		$result = mysql_query($query);
		$result = mysql_fetch_array($result);

		$email = $result['email'];

		// Send an e-mail with the new password.
		$subject = "Reset Password";
		$message = "Your password has been reset for you.\n\nUsername: ".$username."\nNew Password: ".$password."\n\nIf you have any questions, please contact your system administrator.";
		$message = wordwrap($message, 70);
		mail($email, $subject, $message);

		$reset = "true";

	}
	
?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<link href="style.css" rel="stylesheet" type="text/css">
	<title>Reset Your Password</title>
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

<table width="100%" border="0" align="center" cellpadding="0" cellspacing="0">
	<tr>
	<td width="35">&nbsp;</td>
	<td>

		&nbsp;<br>

		<h2>Reset Your Password</h2>

	</td>
	</tr>
	<tr>
	<td></td>
	<td valign="top">

		<table width="600" cellpadding="0" cellspacing="0" border="0">
			<tr>

			<form action="reset_password.php" method="post">
			<input type="hidden" name="username" value="<?php echo $_COOKIE["RDV"]['username']; ?>">

			<td>

				<?php if ( $reset == "true" ) { ?>

				<p>Your password has been successfully reset. Please check your e-mail to receive the new password. When you have received your new password, we suggest that you login and immediately change it. You can change your password in the "Preferences" section of this web application.</p>

				<?php } else { ?>

				<p>If you have forgotten your password, it can be reset. Enter your username below and click "Reset Password." Once your password has been reset, an e-mail will be sent to your on-file e-mail address containing a new password.</p>

				<table cellpadding="5" cellspacing="0" border="0">
					<tr>
					<td width="50">&nbsp;</td>
					<td width="75" align="middle">Username:</td>
					<td align="middle"><input type="text" name="username" size="30"></td>
					</tr>
				</table>

				<p>&nbsp;</p>

				<table cellpadding="5" cellspacing="0" border="0">
					<tr>
					<td width="50">&nbsp;</td>
					<td><input type="submit" name="reset_password" value="Reset Password"></td>
					</tr>
				</table>

				<?php } ?>

				<p>&nbsp;</p>

				<p><a href="index.php">Return to Homepage</a></p>

			</td>

			</form>

			</tr>
		</table>
	
	</td>
	</tr>
</table>


</body>
</html>

