<?php

	session_start();

	// Header information to control browser cache.
	
	header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
	header("Last-Modified: " . gmdate("D, d M Y H:i:s") . " GMT");
	header("Cache-Control: no-store, no-cache, must-revalidate");
	header("Cache-Control: post-check=0, pre-check=0", false);
	header("Pragma: no-cache");
	
	
	// Catches the page from the form.
	
	if ( $_POST["locate"] ) { $page = $_POST["locate"]; }
	elseif ( $_GET["locate"] ) { $page = $_GET["locate"]; }

?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>

<head>
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
	<?php if ( $page != "" ) { ?><link href="style.css" rel="stylesheet" type="text/css"><?php echo "\n"; } ?>
	<title>VE-Suite Data Viewer</title>
	
	<?php if ( $page == "components" ) { ?>
	
	<script language="javascript">
		function regenImage() { parent.overview.location.href="overview.php?page=overview"; }
		function refresh() { frames[0].window.manager.reload(); }
	</script>
	
	<?php } if ( isset($_POST["modifyModule"]) ) { $onLoad=" onLoad=\"parent.frames['overview'].window.location.reload()\""; } ?>
</head>
<?php if ( $page != "" ) { ?><body<?php echo $onLoad; ?>><?php } ?>









<?php if ( $page == "components" ) { ?>

<!-- // COMPONENTS // -->

<table width="100%" border="0" cellpadding="0" cellspacing="0">
	<tr>
	<td width="35">&nbsp;</td>
	<td height="70" valign="top">

		&nbsp;<br>

		<h2>Component Overview</h2>

	</td>
	</tr>
	<tr>
	<td></td>
	<td>
	
		<?php
		
		    include_once( "support/manager.php"); 
			$ID = 0;
			
			// if ( isset($_POST['config']) ) { $ID = $_POST['config']; }
			
			$manager = new PPManager($ID);
			
			// $tableGenString = $manager->getConfigurationListForm();
			
			$tableGenString = $manager->makePullDownMenu();
			
			if ( isset($_GET['component']) ) { $tableGenString .= $manager->modules[$_GET['component']]->drawTable(); }
			elseif ( isset($_POST['component']) ) { $tableGenString .= $manager->modules[$_POST['component']]->drawTable(); }
			else { echo "<p>Select a component by clicking on the component diagram to the right, or from the pulldown menu below.</p>\n"; }
		
			echo $tableGenString;
			
		?>

	</td>
	<td width="15">&nbsp;</td>
	</tr>
</table>









<?php } elseif ($page == "overview" ) { ?>

<!-- // OVERVIEW // -->

<?php

	include_once("support/utility.php");
	insertMapData();
	
?>

<img border="0" src="images/componentOverview.jpg" usemap="#componentMap"> 









<?php } else { ?>

<!-- // FRAMESET // -->

<frameset rows="*" cols="330,*" frameborder="no" border="0" framespacing="0">
	<frame src="overview.php?page=components" name="components" scrolling="auto" noresize>
	<frame src="overview.php?page=overview" name="overview" scrolling="auto" noresize>
	</frameset>
</frameset>

<noframes>
<body>
</body>
</noframes>

<?php } if ( $page != "" ) { ?></body><?php echo "\n"; } ?>
</html>

