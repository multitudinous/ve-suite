<?php

	session_start();
  
	if( ! isset($_SESSION['rdv_chat_id']) ) exit;
  
	$currtime = date("YmdHis",time());
	include_once("../support/configuration.php");
  
	// Maintains this user's state as active.
	mysql_query("UPDATE ".$project_name."chat_users SET last_update = '$currtime' WHERE id = " . $_SESSION['rdv_chat_id']);
  
	// Grab any messages posted since the last time we checked.
	// Notice we say >= and <. This is to guarantee that we don't miss any messages
	// that are posted at the same instant this query is executed.
	$res = mysql_query("SELECT message,username FROM ".$project_name."chat_messages INNER JOIN ".$project_name."chat_users ON ".$project_name."chat_messages.user_id = ".$project_name."chat_users.id WHERE posted >= '" . $_SESSION['rdv_chat_prevtime'] . "' AND posted < '$currtime' ORDER BY posted");

?>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">
<head>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
</head>
<body>

<?php

    if( mysql_num_rows($res) ) {
		echo '<div id="contents">';
		echo '<font face="Arial, Helvetica, sans-serif" size="2">';
		
		while( $row = mysql_fetch_array($res) ) {
			echo '<strong>' . $row['username'] . ': </strong>' . $row['message'] . '<br><img src="../graphics/spacer.gif" width="1" height="10" border="0"><br>';
		}
		
    	$_SESSION['rdv_chat_prevtime'] = $currtime;

    	echo '</font>';
    	echo '</div>';
    	
    }

?>

<script type="text/javascript">
<!--

	// If there are new messages, insert them to the chat contents
	if(parent.insertMessages && document.getElementById("contents"))
		parent.insertMessages(document.getElementById("contents").innerHTML);
      
		setTimeout("getMessages()",1000); //poll server again in one second

		function getMessages(){
        document.location.reload();
		}

//-->
</script>

</body>
</html>