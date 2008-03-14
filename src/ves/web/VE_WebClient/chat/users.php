<?php

	include_once("../support/configuration.php");

?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">
<head>
    <title>Chat</title>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <meta http-equiv="refresh" content="5; url=users.php">
    
    <style>
    
	    body {	font-family: Arial, Helvetica, sans-serif;
				font-size: 11px;
				background-color: #CCFFFF;
				margin-top: 1px;
				margin-left: 3px;
				margin-right: 2px;
	    }
    
    </style>
</head>
<body>

<strong>Current Users</strong>

<p>

<?php

	$query = "SELECT * FROM ".$project_name."chat_users";
	$result = mysql_query($query);
	
	while ( $users = mysql_fetch_array($result) )
	{
		$name = $users["username"];
		echo "$name<br>\n";
	}

?>

</p>

</body>
</html>
