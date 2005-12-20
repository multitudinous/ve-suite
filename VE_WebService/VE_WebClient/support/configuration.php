<?php

$server = "vracs001.vrac.iastate.edu";
$database = "kmbryden";
$uname = "tsvegweb";
$password = "web99";
$project_name = "dv_";

$url = "http://www.vrac.iastate.edu/~bharper/dv/";

mysql_pconnect($server, $uname, $password) or die ("Could not connect to database server. Please try again or contact your system administrator.");
mysql_select_db($database) or die("Could not select database. Please try again or contact your system administrator.");

?>