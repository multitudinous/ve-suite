<?php

	session_start();

	include_once("support/configuration.php");


	// Header information to control browser cache.
	
	header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
	header("Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT");
	header("Cache-Control: no-store, no-cache, must-revalidate");
	header("Cache-Control: post-check=0, pre-check=0", false);
	header("Pragma: no-cache");
	

    include_once( "support/utility.php");
	
	if ( $_POST["locate"] ) { $page = $_POST["locate"]; }
	elseif ( $_GET["locate"] ) { $page = $_GET["locate"]; }
	
	if ( $_POST["edit_attributes"] ) { $page = "edit_attributes"; }
	if ( $_POST["edit_cancel"] ) { $page = "manage"; }
	
	if ( $_POST["edit_update"] ) {
	
		$id 		= $_POST["id"];
		$filename 	= $_POST["filename"];
		$comments 	= $_POST["comments"];
	
		$query = "UPDATE ".$project_name."files SET filename = '$filename', comments = '$comments' WHERE id = '$id'";
		$result = mysql_query($query);
		
		if ( $result ) { $message = "You have successfully edited the attributes for ".$filename; $page = "manage"; }
		else { $error = "There was an unknown error when attempting to update file attributes. Please try again or contact your administrator."; $page = "edit_attributes"; }
	
	}
	
	if ( $delete_file ) {
	
		$id = $_POST["id"];
		
		$query = "DELETE FROM ".$project_name."files WHERE id = '$id'";
		$result = mysql_query($query);
		
		if ( $result ) { $message = "You have successfully deleted the file ".$filename; $page = "manage"; }
		else { $error = "There was an unknown error when attempting to delete the file. Please try again or contact your administrator."; $page = "manage"; }
	
	}

?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<?php if ( $page != "" ) { ?><link href="style.css" rel="stylesheet" type="text/css"><?php echo "\n"; } ?>
	<title>VE-Suite Data Viewer</title>
	
	<?php if ( $page == "manage" ) { ?>
	
	<script language="javascript">
	
		function confirmLink(theLink, file)
		{
		    if (confirmMsg == '' || typeof(window.opera) != 'undefined') { return true; }
			var is_confirmed = confirm(confirmMsg + file + " ?");
		    if (is_confirmed) { theLink.href; }
			return is_confirmed;
		}
		
		var confirmMsg  = 'Are you sure you want to delete the file ';

	</script>
	
	<?php } ?>
</head>
<?php if ( $page != "" ) { ?><body><?php } ?>









<?php if ( $page == "manager" ) { ?>

<!-- // FILE MANAGER // -->

<table width="100%" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td width="35">&nbsp;</td>
	<td height="70" valign="top">

		&nbsp;<br>

		<h2>File Manager</h2>

	</td>
	</tr>
	<tr>
	<td></td>
	<td>

		<p>&#149; <a href="files.php?page=manage" target="overview">Manage Your Files</a></p>
		<p>&#149; <a href="files.php?page=upload" target="overview">Upload Files</a></p>
		<p>&#149; <a href="files.php?page=view_all" target="overview">View All Files</a></p>




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
	
		<p><strong><u>File Manager</u></strong></p>
		
		<p>Choose an option to the left.</p>
	
	</td>
	</tr>
</table>









<?php } elseif ($page == "upload" ) { ?>

<!-- // UPLOAD FILES // -->

<table width="635" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>
	<tr>
	
	<form enctype="multipart/form-data" action="files.php?page=upload" method="post">	
	<input type="hidden" name="user" value="<?php echo $_SESSION["s_name"]; ?>">
	<input type="hidden" name="username" value="<?php echo $_SESSION["s_username"]; ?>">
	
	<td>
	
		<p><strong><u>Upload Files</u></strong></p>

		<?php
	
			if( isset($_POST["uploadSubmitted"]) ) {

				$components 	= $_POST["components"];
				$user			= $_POST["user"];
				$username		= $_POST["username"];
				$comments	 	= $_POST["comments"];
				$fileHandle 	= fopen($fileUpload, "r");
				$fileContent 	= fread($fileHandle, $fileUpload_size);
				$fileContent 	= addSlashes($fileContent);
				$fName 			= $_FILES["fileUpload"]["name"];
				$fType 			= $_FILES["fileUpload"]["type"];
				
				$count = count($components);
				
				for ( $i = 0; $i < $count; $i++ ) {
					if ( $i == 0 ) { $component_list = $components[$i]; }
					else { $component_list .= ", ".$components[$i]; }
				} 	
				
			 	$query = "INSERT INTO ".$project_name."files (filename, components, data, mimeType, user, username, comments) VALUES ('$fName', '$component_list', '$fileContent', '$fType', '$user', '$username', '$comments')";
				$upload = mysql_query($query);
				
				if ( $upload ) { echo "<p><font color=\"#990099\"><strong>The file \"$fName\" has been successfully uploaded.</strong></font></p>\n"; }
				else { echo "<p><font color=\"#CC0000\"><strong>Error: There was an unknown error and the file was not uploaded. Please try again or contact your system administrator.</strong></font></p>\n"; }
				
				fclose($fileHandle);
	
			}
			
			
			include "support/manager.php";
			$ID = 0;
			$manager = new PPManager($ID);
			$menuGenString = $manager->makeSelectionMenu();

		?>
		
		<p>Uploading files allows you to associate files with diagram components. First, choose which components with which to associate this file. To select multiple components, use the Shift or Ctrl keys on your keyboard.</p>
		
		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td><?php echo $menuGenString; ?></td>
			</tr>
		</table>
		
		<p>&nbsp;</p>
		
		<p>Next, choose a file from your computer or network to upload.</p>
		
		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td><input type="file" name="fileUpload" size="50"></td>
			</tr>
		</table>
		
		<p>&nbsp;</p>
		
		<p>You now have an opportunity to leave a discription or comments about this file.</p>
		
		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td><textarea name="comments" rows="3" cols="70" style="font-family: Arial, Helvetica, sans-serif; font-size: 12px;"></textarea></td>
			</tr>
		</table>
		
		<p>&nbsp;</p>
		
		<table cellpadding="5" cellspacing="0" border="0">
			<tr>
			<td width="50">&nbsp;</td>
			<td><input type="submit" value="Upload File" name="uploadSubmitted"></td>
			</tr>
		</table>
	
	</td>
	
	</form>
	
	</tr>
</table>









<?php } elseif ($page == "manage" ) { ?>

<!-- // MANAGE YOUR FILES // -->

<table width="635" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>
	<tr>
	<td>
	
		<p><strong><u>Manage Your Files</u></strong></p>
		
		<?php

			if ( $message || $error ) {

				if ( $message ) { $text = $message; $color = "<font color=\"#000066\">"; }
				elseif ( $error ) { $text = $error; $color = "<font color=\"#FF0000\">"; }
				$end = "</font>";
				
				echo "<p><strong>$color $text $end</strong></p>";
				
			}

		?>
		
		<hr size="1" width="650" align="left" noshade color="#000000">

			<table width="650" cellpadding="4" cellspacing="0" border="0">
					
				<?php

					// Define row colors.

					$color1 = "#EFEFDE"; 
					$color2 = "#FFFFFF"; 
					$username = $_SESSION["s_username"];
					$row_count = 0; 


					// Get information out of content table based on what section is chosen.

					$query = "SELECT * FROM ".$project_name."files WHERE username = '$username' ORDER BY filename ASC";
					$result = mysql_query($query);
					$res = mysql_num_rows($result);
					
					if ( $res > 0 ) {
					
						while ( $display = mysql_fetch_assoc($result) )
						{
								
							$id 			= $display['id'];
							$filename		= $display['filename'];
							$mimeType		= $display['mimeType'];
							$comments		= $display['comments'];
							$color 			= ($row_count % 2) ? $color1 : $color2;
	
							if ( $name == "" ) { $name = "<em>Information Not Available</em>"; }
							if ( $email == "" ) { $email = "<em>Information Not Available</em>"; }
							if ( $last_login == "" ) { $last_login = "<em>Information Not Available</em>"; }

				?>

				<form action="files.php" method="post">
				<input type="hidden" name="id" value="<?php echo "$id"; ?>">
				<input type="hidden" name="filename" value="<?php echo "$filename"; ?>">

				<tr><td width="475" valign="middle" bgcolor="<?php echo "$color"; ?>" colspan="3"><table cellpadding="0" cellspacing="0" border="0"><tr><td width="16" valign="middle"><img src="graphics/file_icon.gif" width="16" height="18" border="0"></td><td align="left" valign="middle">&nbsp;<a href="files_view.php?id=<?php echo $id; ?>" target="file_view" style="font-family: Verdana, Tahoma, sans-serif; font-size: 11px; font-weight: normal; text-decoration: underline;"><?php echo "$filename"; ?></a> &nbsp; (<?php echo "$mimeType"; ?>)</td></tr></table></td><td width="175" align="right" <?php if ( $comments ) { echo "valign=\"bottom\" rowspan=\"2\" "; } else { echo "valign=\"bottom\" "; } ?>bgcolor="<?php echo "$color"; ?>"><input type="submit" name="edit_attributes" value="Edit File Attributes" style="font-size: 10px;">&nbsp;&nbsp;<input type="submit" name="delete_file" value="Delete" style="font-size: 10px;" onclick="return confirmLink( this, '<?php echo $filename; ?>')"></td></tr>
				<?php if ( $comments ) { ?><tr><td width="475" <?php if ( $comments ) { echo "colspan=\"3\" "; } ?> bgcolor="<?php echo "$color"; ?>"><table cellpadding="0" cellspacing="0" border="0"><tr><td width="19" valign="middle"><img src="graphics/spacer.gif" width="19" height="1" border="0"></td><td><?php echo $comments; ?></td></tr></table></td></tr><?php } ?>

				</form>
						
				<?php $row_count++; } } else { ?>
				
				<tr>
				<td>You do not have any files uploaded. <a href="files.php?page=upload" target="overview">Upload Files</a></td>
				</tr>
				
				<?php } ?>

			</table>

			<hr size="1" width="650" align="left" noshade color="#000000">

	
	</td>
	</tr>
</table>









<?php } elseif ($page == "edit_attributes" ) { ?>

<!-- // EDIT ATTRIBUTES // -->

<table width="635" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>
	<tr>
	<td>
	
		<p><strong><u>Edit File Attributes</u></strong></p>
		
		<?php

			if ( $message || $error ) {

				if ( $message ) { $text = $message; $color = "<font color=\"#000066\">"; }
				elseif ( $error ) { $text = $error; $color = "<font color=\"#FF0000\">"; }
				$end = "</font>";
				
				echo "<p><strong>$color $text $end</strong></p>";
				
			}
		
			$id = $_POST["id"];
			
			$query = "SELECT * FROM ".$project_name."files WHERE id = '$id'";
			$result = mysql_query($query);
			$result = mysql_fetch_assoc($result);
			
			$id = $result["id"];
			$filename = $result["filename"];
			$mimeType = $result["mimeType"];
			$components = $result["components"];
			$comments = $result["comments"];
			
		//	echo strpbrk($components, '1');
			
		?>

		<table width="570" cellpadding="0" cellspacing="0" border="0">
			
			<form action="files.php" method="post">
			<input type="hidden" name="id" value="<?php echo $id; ?>">
			<input type="hidden" name="filename" value="<?php echo $filename; ?>">
			
			<tr>
			<td width="220">Filename:</td>
			<td><input type="text" name="filename" size="62" value="<?php echo $filename; ?>" disabled></td>
			</tr>
			<tr>
			<td colspan="2">&nbsp;</td>
			</tr>
			<tr>
			<td>File Type:</td>
			<td><?php echo $mimeType; ?></td>
			</tr>
			<tr>
			<td colspan="2">&nbsp;</td>
			</tr>
			<tr>
			<td valign="top">Associated Component(s):</td>
			<td><?php echo $components; ?>
			
<?php

/*	include "support/manager.php";
	$ID = 0;
	$manager = new PPManager($ID);
	$menuGenString = $manager->makeSelectionMenu();
	echo $menuGenString;
*/
?>
	
			</td>
			</tr>
			<tr>
			<td colspan="2">&nbsp;</td>
			</tr>
			<tr>
			<td valign="top">Comments / Description:</td>
			<td><textarea name="comments" rows="3" cols="70" style="font-family: Arial, Helvetica, sans-serif; font-size: 12px;"><?php echo $comments; ?></textarea></td>
			</tr>
			<tr>
			<td height="50" colspan="2">&nbsp;</td>
			</tr>
			<tr>
			<td colspan="2"><hr width="100%" size="1" noshade color="#000000"></td>
			</tr>
			<tr>
			<td><input type="submit" name="edit_cancel" value="Cancel"></td>
			<td align="right"><input type="submit" name="edit_update" value="Update Attributes"></td>
			</tr>
			<tr>
			<td colspan="2"><hr width="100%" size="1" noshade color="#000000"></td>
			</tr>
			
			</form>
			
		</table>

	</td>
	</tr>
</table>









<?php } elseif ($page == "view_all" ) { ?>

<!-- // VIEW ALL FILES // -->

<table width="635" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>
	<tr>
	<td>
	
		<p><strong><u>View All Files</u></strong></p>
		
		<hr size="1" width="650" align="left" noshade color="#000000">

			<table width="650" cellpadding="4" cellspacing="0" border="0">
					
				<?php

					// Define row colors.

					$color1 = "#EFEFDE"; 
					$color2 = "#FFFFFF"; 
					$username = $_SESSION["s_username"];
					$row_count = 0;

					$query = "SELECT * FROM ".$project_name."files ORDER BY filename ASC";
					$result = mysql_query($query);
					$res = mysql_num_rows($result);
					
					if ( $res > 0 ) {
							
						while ( $display = mysql_fetch_array($result) )
						{
								
							$id 			= $display['id'];
							$filename		= $display['filename'];
							$mimeType		= $display['mimeType'];
							$name			= $display['user'];
							$comments		= $display['comments'];
							$color 			= ($row_count % 2) ? $color1 : $color2;

				?>

				<form action="files.php" method="post">
				<input type="hidden" name="id" value="<?php echo "$id"; ?>">

				<tr><td width="475" valign="middle" bgcolor="<?php echo "$color"; ?>" colspan="3"><table cellpadding="0" cellspacing="0" border="0"><tr><td width="16" valign="middle"><img src="graphics/file_icon.gif" width="16" height="18" border="0"></td><td align="left" valign="middle">&nbsp;<a href="files_view.php?id=<?php echo $id; ?>" target="file_view" style="font-family: Verdana, Tahoma, sans-serif; font-size: 11px; font-weight: normal; text-decoration: underline;"><?php echo "$filename"; ?></a> &nbsp; (<?php echo "$mimeType"; ?>)</td></tr></table></td><td width="175" align="right" <?php if ( $comments ) { echo "valign=\"bottom\" rowspan=\"2\" "; } else { echo "valign=\"middle\" "; } ?>bgcolor="<?php echo "$color"; ?>"><font color="#00CCCC">Uploaded by <?php echo $name; ?></font></td></tr>
				<?php if ( $comments ) { ?><tr><td width="475" <?php if ( $comments ) { echo "colspan=\"3\" "; } ?> bgcolor="<?php echo "$color"; ?>"><table cellpadding="0" cellspacing="0" border="0"><tr><td width="19" valign="middle"><img src="graphics/spacer.gif" width="19" height="1" border="0"></td><td><?php echo $comments; ?></td></tr></table></td></tr><?php } ?>

				</form>
						
				<?php $row_count++; } } else { ?>
				
				<tr>
				<td>There are no files uploaded. <a href="files.php?page=upload" target="overview">Upload Files</a></td>
				</tr>
				
				<?php } ?>

			</table>

			<hr size="1" width="650" align="left" noshade color="#000000">

	
	</td>
	</tr>
</table>









<?php } else { ?>

<!-- // FRAMESET // -->

<frameset rows="*" cols="330,*" frameborder="no" border="0" framespacing="0">
	<frame src="files.php?page=manager" name="manager" scrolling="auto" noresize>
	<frame src="files.php?page=manage" name="overview" scrolling="auto" noresize>
	</frameset>
</frameset>

<noframes>
<body>
</body>
</noframes>

<?php } if ( $page != "" ) { ?></body><?php echo "\n"; } ?>
</html>

