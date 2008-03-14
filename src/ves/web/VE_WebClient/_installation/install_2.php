<?php
	
	if ( ! isset($_SERVER['HTTPS'])) {
		header ("location: index.php" );
	} else {

	include_once("../support/configuration.php");

	$username 			= $_POST['username'];
	$password			= $_POST['password'];
	$verify_password	= $_POST['verify_password'];
	$name				= $_POST['name'];
	$email				= $_POST['email'];

	if ( isset($_POST["step_2"]) ) {
	
		$pattern = '/.*@.*\..*/';
		$error = "";
		
		if ( ! $username ) { $error .= "You must enter a username.<br>"; }
		if ( ! $password ) { $error .= "You must enter a password.<br>"; }
		if ( ! $verify_password ) { $error .= "You must verify your password.<br>"; }
		if ( ! $name ) { $error .= "You must enter your name.<br>"; }
		if ( ! $email ) { $error .= "You must provide your e-mail address.<br>"; }
		if ( $email && ! preg_match($pattern, $email) > 0 ) { $error .= "The e-mail address you entered was not formatted properly.<br>"; }
		if ( $password != $verify_password ) { $error .= "Be sure to enter your desired  password exactly the same both times.<br>"; }
		
		if ( $error == "" ) {

			$query = "INSERT INTO ".$project_name."users SET username = '$username', password = '$password', name = '$name', email = '$email', admin = 'true'";
			$result = mysql_query($query);
	
			if ( ! $result ) { $error = "There was an unknown error when adding this account to the database. Please try again or contact your website administrator."; }
			else {
			
				unset( $username, $password, $verify_password, $name, $email );
				header("location: install_3.php");
				
			}
			
		}
	
	}

	// Header information to control browser cache.
	
	header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
	header("Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT");
	header("Cache-Control: no-store, no-cache, must-revalidate");
	header("Cache-Control: post-check=0, pre-check=0", false);
	header("Pragma: no-cache");
	
	include_once("progress.php");
	
	$step = "2";

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
		
		<form action="install_2.php" method="post">

		&nbsp;<br>

		<table width="615" cellpadding="0" cellspacing="0" border="0">
			<tr>
			<td width="415" align="left" valign="middle"><h2>Step 2 of <?php echo $total; ?></h2></td>
			<td width="250" align="right" valign="middle"><?php progressBar($step, $total); ?></td>
			</tr>
		</table>
		
		<?php if ( $error ) { ?>
					
		<table width="615" cellpadding="7" cellspacing="1" border="0" bgcolor="#CC0000">
			<tr>
			<td bgcolor="#FFFFFF">
			
				<font color="#CC0000">
				<strong>
				
				The following errors occured while attempting attempting to create an administrator account...
				
				<blockquote><?php echo $error; ?></blockquote>
				
				Please try again or contact your system administrator. For security purposes, you must enter your password again.
				
				</strong>
				</font>
			
			</td>
			</tr>
		</table>
		
		<p>&nbsp;</p>
		
		<?php } ?>

		<p><strong><u>Creating an Administrator</u></strong></p>
		
		<p>The VE-Suite Data Viewer requires at least one administrator to be able to add, modify, and delete user accounts. Please provide the following information to create your administration account.</p>
		
		<table width="615" cellpadding="0" cellspacing="0" border="0">
			<tr>
			<td width="25"></td>
			<td width="150">Username:</td>
			<td width="440"><input type="text" name="username" size="60" style="width: 440px;" value="<?php echo $username; ?>"></td>
			</tr>
			<tr>
			<td height="10" colspan="3"></td>
			</tr>
			<tr>
			<td width="25"></td>
			<td width="150">Password:</td>
			<td width="440"><input type="password" name="password" size="60" style="width: 440px;"></td>
			</tr>
			<tr>
			<td height="10" colspan="3"></td>
			</tr>
			<tr>
			<td width="25"></td>
			<td width="150">Verify Password:</td>
			<td width="440"><input type="password" name="verify_password" size="60" style="width: 440px;"></td>
			</tr>
			<tr>
			<td height="30" colspan="3"></td>
			</tr>
			<tr>
			<td width="25"></td>
			<td width="150">Full Name:</td>
			<td width="440"><input type="text" name="name" size="60" style="width: 440px;" value="<?php echo $name; ?>"></td>
			</tr>
			<tr>
			<td height="10" colspan="3"></td>
			</tr>
			<tr>
			<td width="25"></td>
			<td width="150">E-mail Address:</td>
			<td width="440"><input type="text" name="email" size="60" style="width: 440px;" value="<?php echo $email; ?>"></td>
			</tr>
		</table>
		
		<p>&nbsp;</p>
		
		<p>When you press "Continue Installation," your new administration account will be created.</p>
		
		<p>&nbsp;</p>
		
		<p align="right"><input type="submit" name="step_2" value="Continue Installation  -->"></p>
		
		</form>
		
	</td>
	</tr>
</table>


</body>
</html>

<?php } ?>