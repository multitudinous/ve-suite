<?php

	// Kill all values in the session cookie.

	session_start();
	session_unset();
	header ("location: index.php?logout");

?>