<?php

	include_once("support/utility.php");
	include_once("support/configuration.php");

	$query = "SELECT * FROM ".$project_name."files WHERE id = '$id'";
	$result = mysql_query($query);
	
	if( ! $result) die ("There was an unknown error. Please try again or contact your system administrator.");
	
	if( $thisFile = mysql_fetch_array($result) )
	{	
		$head = "Content-type: $thisFile[mimeType]";
		header( $head );
		
		if( isset($_POST[DL]) ) { header("Content-Disposition: attachment; filename=\"$name\""); }
		echo $thisFile[data];
		
	} else  { echo "The file was not found. Please try again or contact your system administrator."; }
	
?>