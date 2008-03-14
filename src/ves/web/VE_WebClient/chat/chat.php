<?php

	include_once("../support/configuration.php");

	if ( $_POST["leave_chat"] ) {
	
		if(isset($_COOKIE[session_name()])){

			session_start();
			mysql_query("DELETE FROM ".$project_name."chat_users WHERE id = " . $_SESSION['rdv_chat_id']);
			$_SESSION = array();
			session_destroy();
			unset($_COOKIE[session_name()]);
			
		}
    
    	echo "<html><head></head><body onload=\"javascript:window.close()\"></body></html>";
    	// echo "<html><head></head><body onLoad=\"setTimeout(window.close, 1000)\"></body></html>";
    	exit;
	
	} else {
	
		session_start();
	    $expiretime = date("YmdHis",time() - 5);
	    $who = $_SESSION["s_name"];
	    
	    if( $who ){
        
	        $result = mysql_query("SELECT id FROM ".$project_name."chat_users WHERE username = '$who' AND last_update > " . $expiretime);
	        
	        if(!mysql_fetch_array($result)){
	        
				mysql_query("DELETE FROM ".$project_name."chat_users WHERE last_update <= " .$expiretime);
				mysql_query("DELETE FROM ".$project_name."chat_messages WHERE posted <= " . $expiretime);
				mysql_query("INSERT INTO ".$project_name."chat_users(username,last_update) VALUES ('$who'," . date("YmdHis",time()).")");
				
				$_SESSION['rdv_chat_id'] = mysql_insert_id();
				$_SESSION['rdv_chat_prevtime'] = date("YmdHis",time());
				
				header("Location: chat.php");
				exit;
        
        	}
		}
	}

	session_start();
	
	// if( ! $_SESSION['rdv_chat_id'] ) { header("Location: ./login.php" ); exit; }
	// elseif( date("YmdHis",time() - 5) > $_SESSION['rdv_chat_prevtime'] ){ header("Location: ./login.php?logout=true"); exit; }

?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">
<head>
    <title>Chat</title>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    
    <style>
    
	    body {	font-family: Arial, Helvetica, sans-serif;
				font-size: 12px;
				background-color: #FFFFFF;
	    }
    
    </style>
    
    <script type="text/javascript">
    <!--
		var cDocument;
		var cWindow;
      
		window.onload = chat_init;
      
		function chat_init(){
		
			// IE 5 (Win/Mac), Konqueror, Safari
			if(window.frames && window.frames["chatContents"])
				cWindow = window.frames["chatContents"];
				
			// IE 5.5+, Mozilla 0.9+, Opera
			else if(document.getElementById("chatContents").contentWindow) 
				cWindow = document.getElementById("chatContents").contentWindow;
			
			// Moz < 0.9 (Netscape 6.0)
			else 
				cWindow = document.getElementById("chatContents");

				// Moz 0.9+, Konq, Safari, IE, Opera
        		if(cWindow.document)
          			cDocument = cWindow.document;
        		
        		// Moz < 0.9 (Netscape 6.0)
        		else 
          			cDocument = cWindow.contentDocument;
      	}
      		
		function insertMessages(content){
			var newDiv = cDocument.createElement("DIV");
			newDiv.innerHTML = content;
			cDocument.getElementById("contents").appendChild(newDiv);
			cWindow.scrollTo(0,cDocument.getElementById("contents").offsetHeight);
      	}

		function resetForm(){
			document.getElementById("message").value = "";
			document.getElementById("message").focus();
      	}
    
    //-->
    </script>
</head>
<body>

<table width="655" height="1%" cellspacing="0" cellpadding="0" border="0">
	<tr>
	
	<form target="post" method="post" action="post.php">
	
	<td width="500">
    
		<iframe id="chatContents" name="chatContents" src="contents.html" width="500" height="250" style="border-top-width: 1px; border-bottom-width: 1px; border-left-width: 1px; border-right-width: 1px; border-style: groove;"></iframe><br>
    
		
		<img src="../graphics/spacer.gif" width="1" height="1" border="0"><br>
		<input type="text" name="message" id="message" style="width: 440px">&nbsp;<input type="submit" value="Send" class="submit">
		    
		<iframe id="post" name="post" src="post.php" width="0" height="0" style="width: 0px; height: 0px; border: 0px;"></iframe>
		<iframe id="thread" name="thread" src="thread.php" width="0" height="0" style="width: 0px; height: 0px; border: 0px;"></iframe>
		
	</td>
	
	</form>
	
	<td width="5"></td>
	
	<form action="chat.php" method="post">
	
	<td width="150" valign="top" align="right">
	
		<iframe id="users" name="users" src="users.php" width="150" height="250" style="border-top-width: 1px; border-bottom-width: 1px; border-left-width: 1px; border-right-width: 1px; border-style: groove;"></iframe>
		
		<img src="../graphics/spacer.gif" width="1" height="1" border="0"><br>
		<input type="submit" name="leave_chat" value="Leave Chat">
		
	</td>
	
	</form>
	
	</tr>
</table>

</body>
</html>
