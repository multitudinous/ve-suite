<?php
  session_start();

  /* make sure the person is logged in. */
  if(!isset($_SESSION['rdv_chat_id']))
    exit;

  if(sizeof($_POST)){ /* make sure something was actually posted. */
    include_once("../support/configuration.php");
    /* replace the previous statements with the appropriate information
    for your system. */

    $expiretime = date("YmdHis",time() - 5);

    mysql_query("DELETE FROM ".$project_name."chat_messages WHERE posted <= '" . 
            $expiretime . "'"); /* delete expired messages. */
    mysql_query("DELETE FROM ".$project_name."chat_users WHERE last_update <= '" . 
            $expiretime. "'"); /* delete inactive participants. */
    mysql_query("INSERT INTO ".$project_name."chat_messages (user_id,posted,message) VALUES(" .
            $_SESSION['rdv_chat_id'] . ",'" . date("YmdHis",time()) . "','" . 
            strip_tags($_POST['message']) . "')"); /* post the message. */

    header("Location: post.php");
    exit;
  }
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <script type="text/javascript"><!--
      if(parent.resetForm)
        parent.resetForm();
      //-->
    </script>
  </head>
</html>