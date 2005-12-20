<?php
	
	@mysql_pconnect("vracs001.vrac.iastate.edu", "tsvegweb", "web99") or die ("Could not connect to server.");
	@mysql_select_db("tsveg") or die("Could not select database.");

?>