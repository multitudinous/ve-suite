<?php
  error_reporting(1);
  include_once("../support/configuration.php");
  
  if($_GET['logout']){ //they are logging out
    if(isset($_COOKIE[session_name()])){
      session_start(); // To be able to use session_destroy
      mysql_query("DELETE FROM ".$project_name."chat_users WHERE id = " . $_SESSION['rdv_chat_id']);
      $_SESSION = array();
      session_destroy(); // To delete the old session file
      unset($_COOKIE[session_name()]);
    }
    header("Location: ./login.php");
    exit;
  }
  

  else {
  	session_start();
    $expiretime = date("YmdHis",time() - 5);
    $who = $_COOKIE["RDV"]["name"];
    if($who){
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
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">
  <head>
    <title>jenChat</title>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  </head>
  <body>
    <h1>jenChat</h1>
    <form class="grid" method="post" action="./login.php">
      Login<br>
      <label for="who">Handle: </label><input type="text" id="who" name="who" value="<? echo str_replace('"','&quot;',stripslashes($_POST['who'])) ?>" />
      <input type="submit" value="Join Chat" class="submit" />
    </form>
    <p class="error">
      <? echo $error; ?>
    </p>
  </body>
</html>