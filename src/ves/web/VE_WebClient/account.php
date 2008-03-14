<?php

	session_start();

	// Header information to control browser cache.
	header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
	header("Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT");
	header("Cache-Control: no-store, no-cache, must-revalidate");
	header("Cache-Control: post-check=0, pre-check=0", false);
	header("Pragma: no-cache");

	include_once("support/configuration.php");




	// -- CHANGE PASSWORD -- //

	if ( $_POST['change_password'] ) {

		$username = $_POST['username'];
		$old_password = $_POST['password'];
		$new_password = $_POST['new_password'];
		$confirm_password = $_POST['confirm_password'];

		$query = "SELECT * FROM ".$project_name."users WHERE username = '$username'";
		$result = mysql_query($query);
		$result = mysql_fetch_array($result);

		$check_password = $result['password'];

		if ( $old_password != $check_password ) { $error = "The current password you entered does not match the password on file. Please try again."; $page = "password"; }
		elseif ( $new_password != $confirm_password ) { $error = "You did not confirm your new password correctly. Please make sure you enter your desired new password exactly the same both times. Please try again."; $page = "password"; }
		else
		{	$query = "UPDATE ".$project_name."users SET password = '$new_password' WHERE username = '$username'";
			$result = mysql_query($query);
			if ( $result ) { $message = "Your password has successfully been changed."; $page = "password"; }
			else { $error = "There was an unknown error. Please try again. If this error persists, contact your system administrator."; $page = "password"; }
		}

	}








	// -- UPDATE INFORMATION -- //

	if ( $_POST['update_information'] ) {

		$username = $_POST['username'];
		$name = $_POST['name'];
		$email = $_POST['email'];
		$pattern = '/.*@.*\..*/';

		if ( $name == "" ) { $error = "Your name is required to use features in this web application. Please try again."; $page = "information"; }
		elseif ( $email == "" ) { $error = "Your e-mail address is required to use features in this web application. Please try again."; $page = "information"; }
		elseif ( ! preg_match($pattern, $email) > 0 ) { $error = "The e-mail address you entered was not formatted properly and was not saved. Please try again."; $page = "information"; }
		else
		{	$query = "UPDATE ".$project_name."users SET email = '$email', name = '$name' WHERE username = '$username'";
			$result = mysql_query($query);
			
			setcookie("RDV[name]", $name);
			setcookie("RDV[email]", $email);
			
			if ( $result ) { $message = "You have successfully updated your personal information."; $page = "information"; }
			else { $error = "There was an unknown error. Please try again. If this error persists, contact your system administrator."; $page = "information"; }
		}

	}

?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<?php if ( $page != "" ) { ?><link href="style.css" rel="stylesheet" type="text/css"><?php echo "\n"; } ?>
	<title>VE-Suite Data Viewer</title>
</head>
<?php if ( $page != "" ) { ?><body><?php } ?>









<?php if ( $page == "menu" ) { ?>

<!-- // MENU // -->

<table width="100%" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td width="35">&nbsp;</td>
	<td height="70" valign="top">

		&nbsp;<br>

		<h2>Your Account</h2>

	</td>
	</tr>
	<tr>
	<td></td>
	<td>

		<p>&#149; <a href="account.php?page=information" target="content">Manage Your Information</a></p>
		<p>&#149; <a href="account.php?page=password" target="content">Manage Your Password</a></p>

	</td>
	</tr>
</table>









<?php } elseif ($page == "overview" ) { ?>

<!-- // OVERVIEW // -->

<table width="635" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>
	<tr>
	<td>

		<p>This part of the application allows you to manage your user account information used in this web application. Choose an option to the left to manage your information.</p>

	</td>
	</tr>
</table>








<?php } elseif ($page == "information" ) { ?>

<!-- // MANAGE YOUR INFORMATION // -->

<table width="635" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>
	<tr>

	<form action="account.php" method="post">
	<input type="hidden" name="username" value="<?php echo $_SESSION["s_username"]; ?>">

	<td height="100%" valign="top">

		<p><strong><u>Manage Your Information</u></strong></p>

		<?php

			if ( $error || $message ) {

				echo "<p><strong>";
			
				if ( $error ) { echo "<font color=\"#CC0000\">Error: " . $error . "</font>"; }
				elseif ( $message ) { echo "<font color=\"#990099\">" . $message . "</font>"; }

				echo "</strong></p>\n";

			}

		?>

		<p>From here, you are able to manage your personal information. Your information is displayed below as it is recorded on file. Please make sure it is up to date. If your information is not up to date, please change the necessary information and click "Update Information" below.</p>

		<?php

			$query = "SELECT * FROM ".$project_name."users WHERE username = '".$_SESSION["s_username"]."'";
			$result = mysql_query($query);
			$result = mysql_fetch_array($result);
			$name = $result['name'];
			$email = $result['email'];

		?>

		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td width="125" align="middle">Full Name:</td>
			<td align="middle"><input type="text" name="name" size="30" value="<?php echo "$name"; ?>"></td>
			</tr>
			<tr>
			<td></td>
			<td width="125" align="middle">E-mail Address:</td>
			<td align="middle"><input type="text" name="email" size="30" value="<?php echo "$email"; ?>"></td>
			</tr>
		</table>

		<p>&nbsp;</p>

		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td><input type="submit" name="update_information" value="Update Information"></td>
			</tr>
		</table>


	</td>

	</form>

	</tr>
</table>








<?php } elseif ($page == "password" ) { ?>

<!-- // MANAGE YOUR PASSWORD // -->

<table width="635" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>
	<tr>

	<form action="account.php" method="post">
	<input type="hidden" name="username" value="<?php echo $_SESSION["s_username"]; ?>">

	<td height="100%" valign="top">

		<p><strong><u>Manage Your Password</u></strong></p>

		<?php

			if ( $error || $message ) {

				echo "<p><strong>";
			
				if ( $error ) { echo "<font color=\"#CC0000\">Error: " . $error . "</font>"; }
				elseif ( $message ) { echo "<font color=\"#990099\">" . $message . "</font>"; }

				echo "</strong></p>\n";

			}

		?>

		<p>From here, you are able to change your password. First, enter your current password for verification.</p>

		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td width="125" align="middle">Current Password:</td>
			<td align="middle"><input type="password" name="password" size="15"></td>
			</tr>
		</table>

		<p>&nbsp;</p>

		<p>Next, enter your desired new password twice. Make sure you enter it exactly the same both times, as this ensures you entered the password you intended.</p>

		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td width="125" align="middle">New Password:</td>
			<td align="middle"><input type="password" name="new_password" size="15"></td>
			</tr>
			<tr>
			<td width="50">&nbsp;</td>
			<td width="125" align="middle">Confirm Password:</td>
			<td align="middle"><input type="password" name="confirm_password" size="15"></td>
			</tr>
		</table>

		<p>&nbsp;</p>

		<p>After you are sure you have entered the new password as desired, click "Change Password" below.</p>

		<p>&nbsp;</p>

		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td><input type="submit" name="change_password" value="Change Password"></td>
			</tr>
		</table>

	</td>

	</form>

	</tr>
</table>








<?php } elseif ($page == "new_user" ) { ?>

<!-- // NEW USER // -->

<table width="635" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>
	<tr>

	<form action="account.php" method="post">
	<input type="hidden" name="username" value="<?php echo $_SESSION["s_username"]; ?>">

	<td height="100%" valign="top">

		<p><strong><u>Manage Your Password</u></strong></p>

		<?php

			if ( $error || $message ) {

				echo "<p><strong>";
			
				if ( $error ) { echo "<font color=\"#CC0000\">Error: " . $error . "</font>"; }
				elseif ( $message ) { echo "<font color=\"#990099\">" . $message . "</font>"; }

				echo "</strong></p>\n";

			}

		?>

		<p>Welcome to VE-Suite Data Viewer. When an account was created for you, you were given a generic password. We suggest you change it to a more secure and personal password now.</p>

		<p>First, enter your generic password for verification.</p>

		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td width="125" align="middle">Current Password:</td>
			<td align="middle"><input type="password" name="password" size="15"></td>
			</tr>
		</table>

		<p>&nbsp;</p>

		<p>Next, enter your desired new password twice. Make sure you enter it exactly the same both times, as this ensures you entered the password you intended.</p>

		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td width="125" align="middle">New Password:</td>
			<td align="middle"><input type="password" name="new_password" size="15"></td>
			</tr>
			<tr>
			<td width="50">&nbsp;</td>
			<td width="125" align="middle">Confirm Password:</td>
			<td align="middle"><input type="password" name="confirm_password" size="15"></td>
			</tr>
		</table>

		<p>&nbsp;</p>

		<p>After you are sure you have entered the new password as desired, click "Change Password" below.</p>

		<p>&nbsp;</p>

		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td><input type="submit" name="change_password" value="Change Password"></td>
			</tr>
		</table>

	</td>

	</form>

	</tr>
</table>









<?php } else { ?>

<!-- // FRAMESET // -->

<frameset rows="*" cols="330,*" frameborder="no" border="0" framespacing="0">
	<frame src="account.php?page=menu" name="menu" scrolling="no" noresize>
	
	<?php if ( $new_user ) { ?>	
	
	<frame src="account.php?page=new_user" name="content" scrolling="auto" noresize>
	
	<?php } else { ?>
	
	<frame src="account.php?page=overview" name="content" scrolling="auto" noresize>
	
	<?php } ?>
	
	</frameset>
</frameset>

<noframes>
<body>
</body>
</noframes>

<?php } if ( $page != "" ) { ?></body><?php echo "\n"; } ?>
</html>

