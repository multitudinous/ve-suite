<?php

	$total = "3";

	function progressBar($step, $total) {
	
		$progress_size = (($step/($total+1))*100);

?>

<table width="250" cellpadding="0" cellspacing="0" border="0">
	<tr>
	<td width="146" align="right" valign="middle" class="status">INSTALLATION PROGRESS:&nbsp;</td>
	<td width="104">

		<table width="104" height="12" cellpadding="1" cellspacing="1" border="0" bgcolor="#CCCCCC">
			<tr>
			<td bgcolor="#FFFFFF"><img src="progress.gif" width="<?php echo $progress_size; ?>" height="8" border="0"></td>
			</tr>
		</table>
		
	</td>
	</tr>
</table>

<?php } ?>