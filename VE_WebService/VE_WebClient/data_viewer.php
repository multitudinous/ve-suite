<?php

	session_start();

	// Header information to control browser cache.
	header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
	header("Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT");
	header("Cache-Control: no-store, no-cache, must-revalidate");
	header("Cache-Control: post-check=0, pre-check=0", false);
	header("Pragma: no-cache");
	
	if ( $_SESSION["s_verify"] == session_id() . $_SERVER["REMOTE_ADDR"] ) {

?>

<html>
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<title>VE-Suite Data Viewer</title>
</head>

<?php if ( isset($new_user) ) { ?>

<frameset rows="130,*" cols="*" frameborder="no" border="0" framespacing="0">
	<frame src="header.php" name="header" scrolling="no" noresize>
	<frame src="account.php?new_user=true" name="main" scrolling="auto" noresize>
	</frameset>
</frameset>

<?php } else { ?>

<frameset rows="130,*" cols="*" frameborder="no" border="0" framespacing="0">
	<frame src="header.php" name="header" scrolling="no" noresize>
	<frame src="overview.php" name="main" scrolling="auto" noresize>
	</frameset>
</frameset>

<?php } ?>

<noframes>
<body>
</body>
</noframes>
</html>

<? } else { header ("location: index.php?session"); }  ?>