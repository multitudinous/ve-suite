<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<link href="../style.css" rel="stylesheet" type="text/css">
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
<body>









<!-- // OVERVIEW // -->

<table width="650" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td height="70">&nbsp;</td>
	</tr>

	<tr>
	<td>

		<p><strong><u>Uninstall VE-Suite Data Viewer</u></strong></p>
		
		<p>Uninstalling the VE-Suite Data Viewer drops all tables from the database used by the VE-Suite Data Viewer.</p>
		
		<p><strong>Uninstalling the VE-Suite Data Viewer is irreversible.</strong> Afterward, you will need to delete the folder and contents of the VE-Suite Data Viewer on your server. If, at any point in the future, the VE-Suite Data Viewer will be reinstalled, you must do a complete clean install.</p>
		
		<p>&nbsp;</p>
		
		<p>&nbsp;</p>
		
		<p>&nbsp;</p>
		
		<p>&nbsp;</p>
		
		<form action="uninstall.php" method="post"><p align="right"><input type="submit" name="submit" value="Continue with Uninstall"></p></form>

	</td>
	</tr>
</table>








</body>
</html>