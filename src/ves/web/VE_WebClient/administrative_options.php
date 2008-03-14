<?php

	session_start();

	// Header information to control browser cache.
	header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
	header("Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT");
	header("Cache-Control: no-store, no-cache, must-revalidate");
	header("Cache-Control: post-check=0, pre-check=0", false);
	header("Pragma: no-cache");

	include_once("support/configuration.php");

	$id			= $_POST['id'];
	$username 	= $_POST['username'];
	$name		= $_POST['name'];
	$email		= $_POST['email'];
	$to			= $_POST['to'];
	$from_name	= $_POST['from_name'];
	$from_email	= $_POST['from_email'];
	$subject	= $_POST['subject'];
	$message	= $_POST['message'];
	
	if ( $_POST['admin'] ) { $admin = $_POST['admin']; }

	$title 			= $_POST['title'];
	$description 	= $_POST['description'];
	$content 		= $_POST['content'];


	// -- ADD NEW USER -- //

	if ( $_POST['add_user'] ) {

		// Insert new user into users table.
		
		$query = "SELECT * FROM ".$project_name."users WHERE username = '$username'";
		$result = mysql_query($query);
		$count = mysql_num_rows($result);
		$pattern = '/.*@.*\..*/';
		
		$allow = "abcdefghijkmnpqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
		srand((double)microtime()*1000000);
		for( $i=0; $i<8; $i++ ) { $pass .= $allow[rand()%strlen($allow)]; }
		
		if ( $count >= 1 ) { $add_error = "There is already another user account with this username. Please try again."; }
		else {
		
			if ( ! $username ) { $add_error = "You must enter a username."; }
			elseif ( ! $name ) { $add_error = "You must provide the full name for the person of this account."; }
			elseif ( ! $email ) { $add_error = "You must provide the e-mail address for the person of this account."; }
			elseif ( ! ctype_alnum($username) ) { $add_error = "Usernames are made of alpha numeric characters only. Please try again."; }
		//	elseif ( ! ctype_alnum($name) ) { $add_error = "Full names must be made of alpha numeric characters only. Please try again."; }
			elseif ( ! preg_match($pattern, $email) > 0 ) { $add_error = "The e-mail address you entered was not formatted properly. Please try again."; }
			else {

				$query = "INSERT INTO ".$project_name."users SET username = '$username', password = '$pass', name = '$name', email = '$email', admin = '$admin', last_login = 'new_user'";
				$result = mysql_query($query);
				
				$url = $_SERVER['PHP_SELF'];
				$url = str_replace( "adminsitrative_options.php", "", $url );
				$url = "http://" . $_SERVER['SERVER_NAME'] . $url;
		
				$subject = "Welcome to VE-Suite Data Viewer";
				$message = $name.",\n\nWelcome to VE-Suite Data Viewer. A new user account has been created for you.\n\nUsername: ".$username."\nPassword: ".$pass."\n\nPlease go to ".$url." to login. For security purposes, you will be prompted to change your password.\n\nIf you have any questions, please contact your system administrator.";
				mail($email, $subject, $message);
				
				$lname = $name;
		
				if ( $result ) { $message = "You have successfully added a user account for ".$lname."."; unset($username, $password, $name, $email, $admin); }
				else { $error = "There was an unknown error when adding the new user. Please try again or contact your website administrator."; }
			
			}
			
		}
		
		$page = "manage_users";

	}




	// -- MAKE ADMINISTRATOR -- //

	elseif ( $_POST['make_administrator'] ) {

		$query = "UPDATE ".$project_name."users SET admin = 'true' WHERE id = '$id' LIMIT 1";
		$result = mysql_query($query);

		if ( $result ) { $message = "You have successfully made ".$name." an administrator."; $page = "manage_users"; }
		else { $error = "There was an unknown error when changing administrative permissions. Please try again or contact your website administrator."; $page = "manage_users"; }

	}




	// -- MAKE NON-ADMINISTRATOR -- //

	elseif ( $_POST['make_non_administrator'] ) {

		$query = "UPDATE ".$project_name."users SET admin = 'false' WHERE id = '$id' LIMIT 1";
		$result = mysql_query($query);

		if ( $result ) { $message = "You have successfully made ".$name." a non-administrator."; $page = "manage_users"; }
		else { $error = "There was an unknown error when changing administrative permissions. Please try again or contact your website administrator."; $page = "manage_users"; }

	}




	// -- DESTROY USER -- //

	elseif ( $_POST['destroy_user'] ) {

		$query = "DELETE FROM ".$project_name."users WHERE id = '$id' LIMIT 1";
		$result = mysql_query($query);

		if ( $result ) { $message = "You have successfully deleted ".$name."'s user account."; $page = "manage_users"; }
		else { $error = "There was an unknown error when attempting to delete this user. Please try again or contact your website administrator."; $page = "manage_users"; }

	}




	// -- E-MAIL USERS -- //
	
	elseif ( $_POST['email_users'] ) {
	
		if ( $to == "admin" ) { $woo = " WHERE admin = 'true'"; }
		elseif ( $to == "nonadmin" ) { $woo = " WHERE admin = 'false'"; }
		
		$count = 0;
		$message = wordwrap($message, 70);
	
		$query = "SELECT * FROM rdv_users" . $woo;
		$result = mysql_query($query);
		
		while ($send = mysql_fetch_array($result))
		{
			$to = $send['email'];
			
			if ( $count == 0 ) { $sendto = $to; }
			else { $sendto .= ", " . $to; }
			
			$count ++;
			
		}

		$headers = 'From: ' . $from_name . ' <' . $from_email . '>' . "\r\n";
		$headers .= 'Bcc: ' . $sendto .  "\r\n";

		mail($from_email, $subject, $message, $headers);
		
		$page = "email_users";
		$message = "Your e-mail has been sent.";

	}

?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<?php if ( $page != "" ) { ?><link href="style.css" rel="stylesheet" type="text/css"><?php echo "\n"; } ?>
	<title>VE-Suite Data Viewer</title>
	
	<script language="javascript">
	
		function confirmDelete(theLink, account){
			if (entryMsg == '' || typeof(window.opera) != 'undefined') { return true; }
			var is_confirmed = confirm(entryMsg + account + " ?");
		    if (is_confirmed) { theLink.href; }
			return is_confirmed;
		}
		
		var entryMsg  = 'Are you sure you want to delete the user account for ';

	</script>
</head>
<?php if ( $page != "" ) { ?><body><?php } ?>









<?php if ( $page == "menu" ) { ?>

<!-- // MENU // -->

<table width="100%" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td width="35">&nbsp;</td>
	<td height="70" valign="top">

		&nbsp;<br>

		<h2>Administrative Options</h2>

	</td>
	</tr>
	<tr>
	<td></td>
	<td>

		<p>&#149; <a href="administrative_options.php?page=manage_users" target="content">Manage Users</a></p>
		<p>&#149; <a href="administrative_options.php?page=email_users" target="content">E-mail Users</a></p>
		
		<p>&nbsp;</p>
		
		<p>&#149; <a href="_uninstall/index.php" target="content" class="uninstall">Uninstall VE-Suite Data Viewer</a></p>

	</td>
	</tr>
</table>









<?php } elseif ( $page == "overview" ) { ?>

<!-- // OVERVIEW // -->

<table width="650" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>
	<tr>
	<td>

		<p>This part of the application allows administrators to update administrative options. Choose an option to the left.</p>

	</td>
	</tr>
</table>









<?php } elseif ($page == "manage_users" ) { ?>

<!-- // MANAGE USERS // -->

<table width="650" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>
	<tr>
	<td>

		<?php

			if ( $message || $error ) {

				if ( $message ) { $text = $message; $begin = "<font color=\"#000066\">"; }
				elseif ( $error ) { $text = $error; $begin = "<font color=\"#FF0000\">"; }
				$end = "</font>";

		?>

			<p><strong><?php echo $begin . $text . $end; ?></strong></p>
			
			<p>&nbsp;</p>

		<?php } ?>


	<form action="administrative_options.php" method="post">

	<p><strong><u>Add Users</u></strong></p>
	
	<hr size="1" width="650" align="left" noshade color="#000000">

	<table width="100%" cellpadding="4" cellspacing="0" border="0" bgcolor="#EFEFDE">
		<tr>
		<td colspan="3">
		
			<?php
				
				if ( $add_error ) { echo "<font color=\"#FF0000\"><strong>Error: " . $add_error . "</strong></font>"; }
				else { echo "Please provide the following information to add a new user. Once added, an e-mail will be sent to the new user informing them of their account information. All fields are required."; }
				
				echo "<br>&nbsp;";
		
			?>
		
		</td>
		</tr>
		<tr>
		<td width="125" valign="middle">Account Type:</td>
		<td valign="middle">

			<select name="admin">
				<option value="true" <?php if ( $admin == "true" ) { echo "selected"; } ?>>Administrative</option>
				<option value="false" <?php if ( $admin != "true" ) { echo "selected"; } ?>>Non-Administrative</option>
			</select>

		</td>
		<td></td>
		</tr>
		<tr>
		<td width="125" valign="middle">Username:</td>
		<td width="200" valign="middle"><input type="text" name="username" size="30" value="<?php echo $username; ?>"></td>
		<td></td>
		</tr>
		<tr>
		<td width="125" valign="middle">Full Name:</td>
		<td valign="middle"><input type="text" name="name" size="30" value="<?php echo $name; ?>"></td>
		<td></td>
		</tr>
		<tr>
		<td width="125" valign="middle">E-mail Address:</td>
		<td valign="middle"><input type="text" name="email" size="30" value="<?php echo $email; ?>"></td>
		<td align="right"><input type="submit" name="add_user" value="Add New User"></td>
		</tr>
	</table>

	</form>
	
	<hr size="1" width="650" align="left" noshade color="#000000">
	
	<p>&nbsp;</p>

		<!-- // MAKE A LISTING OF NON-ADMINISTRATIVE USERS // -->

		<p><strong><u>Non-Administrative Users</u></strong></p>

			<hr size="1" width="650" align="left" noshade color="#000000">

			<table width="650" cellpadding="4" cellspacing="0" border="0">
					
				<?php

					// Define row colors.

					$color1 = "#EFEFDE"; 
					$color2 = "#FFFFFF"; 
					$row_count = 0; 


					// Get information out of content table based on what section is chosen.

					$query = "SELECT * FROM ".$project_name."users WHERE admin = 'false' ORDER BY username ASC";
					$result = mysql_query($query);
					$count = mysql_num_rows($result);
					
					if ( $count > 0 ) {
							
						while ( $display = mysql_fetch_array($result) )
						{
								
							$id 			= $display['id'];
							$username		= $display['username'];
							$name			= $display['name'];
							$email			= $display['email'];
							$last_login		= $display['last_login'];
							$color 			= ($row_count % 2) ? $color1 : $color2;
	
							if ( $name == "" ) { $name = "<em>Information Not Available</em>"; }
							if ( $email == "" ) { $email = "<em>Information Not Available</em>"; }
							if ( $last_login == "new_user" || $last_login == "" ) { $last_login = "<em>Information Not Available</em>"; }

				?>
					
				<tr>

				<form action="administrative_options.php" method="post">
				<input type="hidden" name="id" value="<?php echo "$id"; ?>">
				<input type="hidden" name="name" value="<?php echo "$name"; ?>">

				<td width="270" align="left" valign="top" bgcolor="<?php echo "$color"; ?>">Username: <?php echo "$username" ?><br>Last Login: <?php echo "$last_login" ?></td>
				<td width="200" align="left" valign="top" bgcolor="<?php echo "$color"; ?>">Name: <?php echo "$name" ?><br>E-mail: <?php echo "$email" ?></td>
				<td width="210" align="right" valign="middle" bgcolor="<?php echo "$color"; ?>"><input type="submit" name="make_administrator" value="Make Administrator" style="font-size: 10px;">&nbsp;&nbsp;<input type="submit" name="destroy_user" value="Delete" style="font-size: 10px;" onclick="javascript:return confirmDelete( '<?php echo $name . " ( " . $username . " ) "; ?>' )"></td>

				</form>

				</tr>
						
				<?php $row_count++; } } else { ?>
				
				<tr><td>There are no users to display.</td></tr>
				
				<?php } ?>

			</table>

			<hr size="1" width="650" align="left" noshade color="#000000">

		<p>&nbsp;</p>

		<!-- // MAKE A LISTING OF ADMINISTRATIVE USERS // -->

		<p><strong><u>Administrative Users</u></strong></p>
					
			<hr size="1" width="650" align="left" noshade color="#000000">

			<table width="650" cellpadding="4" cellspacing="0" border="0">
					
				<?php

					// Define row colors.

					$color1 = "#EFEFDE"; 
					$color2 = "#FFFFFF"; 
					$row_count = 0; 


					// Get information out of content table based on what section is chosen.

					$query = "SELECT * FROM ".$project_name."users WHERE admin = 'true' ORDER BY username ASC";
					$result = mysql_query($query);
							
					while ( $display = mysql_fetch_array($result) )
					{
							
						$id 			= $display['id'];
						$username		= $display['username'];
						$name			= $display['name'];
						$email			= $display['email'];
						$last_login		= $display['last_login'];
						$color 			= ($row_count % 2) ? $color1 : $color2;

						if ( $name == "" ) { $name = "<em>Information Not Available</em>"; }
						if ( $email == "" ) { $email = "<em>Information Not Available</em>"; }
						if ( $last_login == "new_user" || $last_login == "" ) { $last_login = "<em>Information Not Available</em>"; }

				?>
					
				<tr>

				<form action="administrative_options.php" method="post">
				<input type="hidden" name="id" value="<?php echo "$id"; ?>">
				<input type="hidden" name="name" value="<?php echo "$name"; ?>">

				<td width="270" align="left" valign="top" bgcolor="<?php echo "$color"; ?>">Username: <?php echo "$username" ?><br>Last Login: <?php echo "$last_login" ?></td>
				<td width="200" align="left" valign="top" bgcolor="<?php echo "$color"; ?>">Name: <?php echo "$name" ?><br>E-mail: <?php echo "$email" ?></td>
				<td width="210" align="right" valign="middle" bgcolor="<?php echo "$color"; ?>"><input type="submit" name="make_non_administrator" value="Make Non-Administrator" style="font-size: 10px;">&nbsp;&nbsp;<input type="submit" name="destroy_user" value="Delete" style="font-size: 10px;" <?php if ( $_SESSION["s_username"] == $username ) { echo "disabled"; } ?> onclick="javascript:return confirmDelete( '<?php echo $name . " ( " . $username . " ) "; ?>' )"></td>

				</form>

				</tr>
						
				<?php $row_count++; } ?>

			</table>

			<hr size="1" width="650" align="left" noshade color="#000000">

	</td>
	</tr>
</table>









<?php } elseif ( $page == "email_users" ) { ?>

<!-- // E-MAIL USERS // -->

<table width="650" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>
	<tr>

	<form action="administrative_options.php" method="post">
	<input type="hidden" name="from_name" value="<?php echo $_SESSION["s_name"]; ?>">
	<input type="hidden" name="from_email" value="<?php echo $_SESSION["s_email"]; ?>">

	<td>

		<p><strong><u>E-mail Users</u></strong></p>

		<?php

			if ( $message || $error ) {

				if ( $message ) { $text = $message; $color = "<font color=\"#000066\">"; }
				elseif ( $error ) { $text = $error; $color = "<font color=\"#FF0000\">"; }
				$end = "</font>";	

		?>

			<p><strong><?php echo "$color" . "$text" . "$end"; ?></strong></p>
			
			<p>&nbsp;</p>

		<?php } ?>

		<table cellpadding="0" cellspacing="0" border="0">			
			<tr>
			<td width="100" valign="middle">To:</td>
			<td valign="middle">

				<select name="to">
					<option selected></option>
					<option value="admin">Administrative Users</option>
					<option value="nonadmin">Non-Administrative Users</option>
					<option value="all">All Users</option>
				</select>

			</td>
			</tr>
			<tr>
			<td height="10" colspan="2"></td>
			</tr>
			<tr>
			<td width="100" valign="middle">From:</td>
			<td valign="middle"><?php echo $_SESSION["s_name"] . " (" . $_SESSION["s_email"] . ")"; ?></td>
			</tr>
			<tr>
			<td height="10" colspan="2"></td>
			</tr>
			<tr>
			<td width="100" valign="middle">Subject:</td>
			<td valign="middle"><input type="text" name="subject" size="60" style="font-family: Arial, sans-serif; font-size: 12px; width: 550px;"></td>
			</tr>
			<tr>
			<td height="50" colspan="2"></td>
			</tr>
			<tr>
			<td valign="top"><img src="graphics/spacer.gif" width="1" height="3" border="0"><br>Message:</td>
			<td><textarea name="message" rows="10" cols="30" style="font-family: Arial, sans-serif; font-size: 12px; width: 550px;"></textarea></td>
			</tr>
		</table>

		<p>&nbsp;</p>

		<table width="100%" cellpadding="0" cellspacing="0" border="0">
			<tr>
			<td align="right"><input type="submit" name="email_users" value="Send E-mail"></td>
			</tr>
		</table>

	</td>

	</form>

	</tr>
</table>










<?php } else { ?>

<!-- // FRAMESET // -->

<frameset rows="*" cols="330,*" frameborder="no" border="0" framespacing="0">
	<frame src="administrative_options.php?page=menu" name="menu" scrolling="no" noresize>
	<frame src="administrative_options.php?page=overview" name="content" scrolling="auto" noresize>
	</frameset>
</frameset>

<noframes>
<body>
</body>
</noframes>

<?php } if ( $page != "" ) { ?></body><?php echo "\n"; } ?>
</html>

