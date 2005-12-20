<?php session_start(); ?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<link href="style.css" rel="stylesheet" type="text/css">
	
	<script language="javascript">
	<!--

		function chat(URL) {
			day = new Date();
			id = day.getTime();
			eval("page" + id + " = window.open(URL, '" + id + "', 'toolbar=0,scrollbars=0,location=0,statusbar=0,menubar=0,resizable=0,width=670,height=290');");
		}

		function vv(URL) {
			day = new Date();
			id = day.getTime();
			eval("page" + id + " = window.open(URL, '" + id + "', 'toolbar=0,scrollbars=0,location=0,statusbar=0,menubar=0,resizable=0,modal=yes,width=622,height=750');");
		}
		
	// -->
	</script>

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
	<tr>
	<td height="80"></td>
	<td height="80" colspan="2" valign="middle">
	
		<h1>VE-Suite Data Viewer</h1>
		
	</td>
	</tr>
	</tr>
	<tr>
	<td height="1" colspan="3" bgcolor="#000000"></td>
	</tr>
	<tr>
	<td height="20"></td>
	<td height="20">

		<font class="header">

		|&nbsp; &nbsp; &nbsp; 
		<a href="overview.php" target="main">Component Overview</a> &nbsp; &nbsp; &nbsp;|&nbsp; &nbsp; &nbsp; 
		<a href="javascript:vv('vv/vv.php')">Virtual Viewer</a> &nbsp; &nbsp; &nbsp;|&nbsp; &nbsp; &nbsp; 
		<a href="javascript:chat('chat/chat.php')">Launch Chat</a> &nbsp; &nbsp; &nbsp;|&nbsp; &nbsp; &nbsp; 
		<a href="files.php" target="main">File Manager</a> &nbsp; &nbsp; &nbsp;|&nbsp; &nbsp; &nbsp; 
		<a href="account.php" target="main">Your Account</a> &nbsp; &nbsp; &nbsp;|&nbsp; &nbsp; &nbsp; 
		<?php if ( $_SESSION["s_admin"] == "true" ) { ?><a href="administrative_options.php" target="main">Administrative Options</a> &nbsp; &nbsp; &nbsp;|&nbsp; &nbsp; &nbsp; <?php } ?>

		</font>

	</td>
	<td align="right">

		<b><a href="logout.php" target="_top">Logout</a>&nbsp;&nbsp;&nbsp;</b>

	</td>
	</tr>
	<tr>
	<td height="1" colspan="3" bgcolor="#000000"></td>
	</tr>
</table>

</body>
</html>
