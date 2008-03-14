<?php
	
	if ( ! isset($_SERVER['HTTPS'])) {
		header ("location: index.php" );
	} else {

	include_once("../support/configuration.php");

	if ( isset($_POST["add_user"]) ) {
	
		$username 	= $_POST['username'];
		$password	= "gocyclones";
		$name		= $_POST['name'];
		$email		= $_POST['email'];
		$pattern 	= "/.*@.*\..*/";
		$error 		= "";
		
		if ( ! $username ) { $error .= "You must enter a username.<br>"; }
		
		if ( $username ) {
			$query = "SELECT * FROM ".$project_name."users WHERE username = '$username'";
			$result = mysql_query($query);
			$count = mysql_num_rows($result);
			if ( $count >= 1 ) { $error .= "There is already a user account with that username.<br>"; }
		}
		
		if ( ! $name ) { $error .= "You must enter a name.<br>"; }
		if ( ! $email ) { $error .= "You must provide an e-mail address.<br>"; }
		if ( $email && ! preg_match($pattern, $email) > 0 ) { $error .= "The e-mail address you entered was not formatted properly.<br>"; }
		
		if ( $error == "" ) { 
		
			// Insert new user into users table.
	
			$query = "INSERT INTO ".$project_name."users SET username = '$username', password = '$password', name = '$name', email = '$email', admin = '$admin'";
			$result = mysql_query($query);
	
			// Send the new user an e-mail.
			
			$headers = "From: Remote Data Viewer <no-reply>\r\n";
	
			$subject = "DOE Interactive Power Plant Remote Data Viewer";
			$message = $name.",\n\nWelcome to the DOE Interactive Power Plant Remote Data Viewer web application. A new user account has been created for you.\n\nUsername: ".$username."\nPassword: gocyclones\n\nPlease go to ".$url." to login. For security purposes, you will be prompted to change your password.\n\nIf you have any questions, please contact your system administrator.";
			$message = wordwrap($message, 90);
			mail($email, $subject, $message, $headers);
	
			if ( $result ) { $message = "You have successfully added a user account for ".$name."."; unset( $username, $password, $name, $email); }
			else { $error = "There was an unknown error when adding the new user. Please try again or contact your website administrator."; }
			
		}
	
	} elseif ( isset($_POST["complete_installation"]) ) {
	
		header( "location: installation_complete.php" );
	
	}

	// Header information to control browser cache.
	
	header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
	header("Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT");
	header("Cache-Control: no-store, no-cache, must-revalidate");
	header("Cache-Control: post-check=0, pre-check=0", false);
	header("Pragma: no-cache");
	
	include_once("progress.php");
	
	$step = "3";

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
			<td width="415" align="left" valign="middle"><h2>Step 3 of <?php echo $total; ?></h2></td>
			<td width="250" align="right" valign="middle"><?php progressBar($step, $total); ?></td>
			</tr>
		</table>

		<?php if ( $error ) { ?>
					
		<table width="615" cellpadding="7" cellspacing="1" border="0" bgcolor="#CC0000">
			<tr>
			<td bgcolor="#FFFFFF">
			
				<font color="#CC0000">
				<strong>
				
				The following errors occured while attempting attempting to create a new user account...
				
				<blockquote><?php echo $error; ?></blockquote>
				
				Please try again.
				
				</strong>
				</font>
			
			</td>
			</tr>
		</table>
		
		<p>&nbsp;</p>
		
		<?php } ?>

		<p><strong><u>Adding Additional Users</u></strong></p>
		
		<?php

			if ( $message ) { echo "<p><strong><font color=\"#000066\">".$message."</font></strong></p>"; }
			elseif ( ! $message || ! $error ) { echo "<p>You may now take this opportunity to add new user accounts to the VE-Suite Data Viewer.</p>"; }

		?>
		
		<p>To add a new user account, fill out the following information and click "Add User." When you add a new user, they are automatically given the generic password "gocyclones." An e-mail will be sent to the new user containing this information. When they first log in, they will be prompted to change their password.</p>

		<p>You may finish installation at any time by clicking "Complete Installation" below.</p>
		
		<p>&nbsp;</p>
		
		<table width="615" cellpadding="0" cellspacing="0" border="0">
		
			<form action="install_3.php" method="post">
		
			<tr>
			<td width="25"></td>
			<td width="150">Username:</td>
			<td width="440" colspan="2"><input type="text" name="username" size="60" style="width: 440px;" value="<?php echo $username; ?>"></td>
			</tr>
			<tr>
			<td height="10" colspan="4"></td>
			</tr>
			<tr>
			<td width="25"></td>
			<td width="150">Full Name:</td>
			<td width="440" colspan="2"><input type="text" name="name" size="60" style="width: 440px;" value="<?php echo $name; ?>"></td>
			</tr>
			<tr>
			<td height="10" colspan="4"></td>
			</tr>
			<tr>
			<td width="25"></td>
			<td width="150">E-mail Address:</td>
			<td width="440" colspan="2"><input type="text" name="email" size="60" style="width: 440px;" value="<?php echo $email; ?>"></td>
			</tr>
			<tr>
			<td height="10" colspan="4"></td>
			</tr>
			<tr>
			<td width="25"></td>
			<td width="150"><a href="faq.php#accounttypes" class="faq" target="new">Account Type</a>:</td>
			<td width="340">
			
				<select name="admin">
					<option value="true"<?php if ( $admin == "true" ) { echo " selected"; } ?>>Administrative</option>
					<option value="false"<?php if ( $admin != "true" ) { echo " selected"; } ?>>Non-Administrative</option>
				</select>
			
			</td>
			<td width="100" align="right"><input type="submit" name="add_user" value="Add User"></td>
			</tr>
			
			</form>
			
		</table>
		
		<p>&nbsp;</p>
		
		<p>&nbsp;</p>
		
		<form action="install_3.php" method="post">
		
		<p align="right"><input type="submit" name="complete_installation" value="Complete Installation  -->"></p>
		
		</form>
		
	</td>
	</tr>
</table>


</body>
</html>

<?php } ?>