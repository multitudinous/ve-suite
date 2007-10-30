<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE xsl:stylesheet [ <!ENTITY nbsp "&#160;"> ]>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"  xmlns:xt="http://www.jclark.com/xt">

<xsl:output method="html"/>

<xsl:template match="/">
	<html xmlns:fo="http://www.w3.org/1999/XSL/Format">
	<head>
	<script type="text/javascript" src="overlib.js"></script>
<script language="JavaScript1.1">
&lt;!--

function wopen(va){
	var vat = va;
	var url='';
	var height = 600;
	var width = 800;
	var wname = 'n';
	var winleft = (screen.width - width) / 2;
	var winUp = (screen.height - height) / 2;
	var WO1;
	winProp = 'width='+width+',height='+height+',left='+winleft+',top='+winUp+',scrollbars,resizable,toolbar=no,location=no, directories=no'
	WO1=window.open(url, wname, winProp);
	WO1.focus();
	WO1.document.write(vat);
	WO1.document.close();
}

//------&gt;
</script>
	<link href="style.css" rel="stylesheet" type="text/css" />
	</head>
		<body>
			<xsl:apply-templates/>
		</body>
	</html>
</xsl:template>

<xsl:attribute-set name="tb.basics">
   <xsl:attribute name="id"><xsl:value-of select="name"/></xsl:attribute>
   <xsl:attribute name="style">position:absolute; left:<xsl:value-of select="iconLocation/xLocation"/>px; top:<xsl:value-of select="iconLocation/yLocation"/>px; width:10px; z-index:1; visibility: visible</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="ports.basics">
   <xsl:attribute name="id">id</xsl:attribute>
   <xsl:attribute name="style">position:absolute; left:<xsl:value-of select="xLocation"/>px; top:<xsl:value-of select="yLocation"/>px; width:1px; z-index:2; visibility: visible</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="a.basics">
   <xsl:attribute name="href">javascript:wopen('<xsl:value-of select="name"/>')</xsl:attribute>
   <xsl:attribute name="onMouseOver">return overlib('Model Name : <xsl:value-of select="name"/>&lt;br /&gt; ID : <xsl:value-of select="ID"/> &lt;br /&gt;');</xsl:attribute>
   <xsl:attribute name="onMouseOut">return nd();</xsl:attribute>
</xsl:attribute-set>

<xsl:template match="link">
	<table><tr><td><b>&lt;Link Points&gt;</b></td></tr></table>
	<xsl:apply-templates select="linkPoints"/>
</xsl:template>

<xsl:template match="veModel">
<div xsl:use-attribute-sets="tb.basics">
<a xsl:use-attribute-sets="a.basics">
  <table width="10" height="10" cellspacing="1" cellpadding='1' bgcolor='gray'>
		<tr><td align="center"></td></tr>
  </table>
</a>
  <table width='40' height="10" cellspacing='0' cellpadding='0'>
		<tr><td align="left" class="s_text"><xsl:value-of select="name"/></td></tr>
  </table>
  <table width='40' height="10" cellspacing='0' cellpadding='0'>
		<tr><td align="left" class="st_text">(<xsl:value-of select="iconLocation/xLocation"/>,<xsl:value-of select="iconLocation/yLocation"/>)</td></tr>
  </table>
</div>
</xsl:template>


<xsl:template match="linkPoints">

<xsl:variable name="x1">
	<xsl:value-of select="xLocation"/>
</xsl:variable>

<xsl:variable name="x2">
       <xsl:choose>
		<xsl:when test="boolean(following-sibling::linkPoints/xLocation)">
			<xsl:value-of select="following-sibling::linkPoints/xLocation"/>
		</xsl:when>
		<xsl:when test="not(following-sibling::linkPoints/xLocation)">
			<xsl:value-of select="xLocation"/>
		</xsl:when>
       </xsl:choose>
</xsl:variable>

<xsl:variable name="y1">
	<xsl:value-of select="yLocation"/>
</xsl:variable>

<xsl:variable name="y2">
       <xsl:choose>
		<xsl:when test="boolean(following-sibling::linkPoints/yLocation)">
			<xsl:value-of select="following-sibling::linkPoints/yLocation"/>
		</xsl:when>
		<xsl:when test="not(following-sibling::linkPoints/yLocation)">
			<xsl:value-of select="yLocation"/>
		</xsl:when>
       </xsl:choose>
</xsl:variable>

<xsl:variable name="xValue">
		<xsl:value-of select="$x2 - $x1"/>
</xsl:variable>

<xsl:variable name="yValue">
		<xsl:value-of select="$y2 - $y1"/>
</xsl:variable>

<xsl:variable name="xPos">
       <xsl:choose>
         <xsl:when test="$xValue = 0">1</xsl:when>
         <xsl:when test="$xValue &lt; 0"><xsl:value-of select="$xValue * -1"/></xsl:when>
         <xsl:when test="not(($xValue = 0)and($xValue &lt; 0))">
		 	<xsl:value-of select="$xValue"/>
		 </xsl:when>
       </xsl:choose>
</xsl:variable>

<xsl:variable name="yPos">
       <xsl:choose>
         <xsl:when test="$yValue = 0">1</xsl:when>
         <xsl:when test="$yValue &lt; 0"><xsl:value-of select="$yValue * -1"/></xsl:when>
         <xsl:when test="not(($yValue = 0)and($yValue &lt; 0))">
		 	<xsl:value-of select="$yValue"/>
		 </xsl:when>
       </xsl:choose>
</xsl:variable>

<xsl:variable name="xLoc">
       <xsl:choose>
         <xsl:when test="$xValue &lt; 0">
	   		<xsl:if test="boolean(following-sibling::linkPoints/xLocation)">
				<xsl:value-of select="following-sibling::linkPoints/xLocation"/>
			</xsl:if>
		 </xsl:when>
         <xsl:when test="not($xValue &lt; 0)">
		 	<xsl:value-of select="$x1"/>
		 </xsl:when>
	   </xsl:choose>
</xsl:variable>

<xsl:variable name="yLoc">
       <xsl:choose>
         <xsl:when test="$yValue &lt; 0">
	   		<xsl:if test="boolean(following-sibling::linkPoints/yLocation)">
				<xsl:value-of select="following-sibling::linkPoints/yLocation"/>
			</xsl:if>
		 </xsl:when>
         <xsl:when test="not($yValue &lt; 0)">
		 	<xsl:value-of select="$y1"/>
		 </xsl:when>
	   </xsl:choose>
</xsl:variable>

<xsl:variable name="x">
		<xsl:if test="boolean(preceding-sibling::linkPoints[1]/xLocation)">
			<xsl:value-of select="preceding-sibling::linkPoints[1]/xLocation"/>
		</xsl:if>
		<xsl:if test="not(preceding-sibling::linkPoints[1]/xLocation)">
			<xsl:value-of select="xLocation"/>
		</xsl:if>
</xsl:variable>

<xsl:variable name="y">
       <xsl:choose>
		<xsl:when test="boolean(preceding-sibling::linkPoints[1]/yLocation)">
			<xsl:value-of select="preceding-sibling::linkPoints[1]/yLocation"/>
		</xsl:when>
		<xsl:when test="not(preceding-sibling::linkPoints[1]/yLocation)">
			<xsl:value-of select="yLocation"/>
		</xsl:when>
       </xsl:choose>
</xsl:variable>

<xsl:variable name="diffX">
		<xsl:value-of select="$xLoc - $x"/>
</xsl:variable>

<xsl:variable name="diffY">
		<xsl:value-of select="$yLoc - $y"/>
</xsl:variable>

		<xsl:choose>
		<xsl:when test="(($xValue = 0) and ($yValue = 0))">
		   	<xsl:if test="(($diffX &gt; 0) and ($diffY = 0))">
			<div id="id3" style="position:absolute; left:{$x2}px; top:{($y2 - 6)}px; z-index:2; visibility: visible">
				<table><tr><td><img src="a_right.png"></img></td></tr></table>
			</div>
			</xsl:if>
		   	<xsl:if test="(($diffX &lt; 0) and ($diffY = 0))">
			<div id="id3" style="position:absolute; left:{$x2}px; top:{($y2 - 6)}px; z-index:2; visibility: visible">
				<table><tr><td><img src="a_left.png"></img></td></tr></table>
			</div>
			</xsl:if>
		   	<xsl:if test="(($diffX = 0) and ($diffY &lt; 0))">
			<div id="id3" style="position:absolute; left:{($x2 - 6)}px; top:{$y2}px; z-index:2; visibility: visible">
				<table><tr><td><img src="a_up.png"></img></td></tr></table>
			</div>
			</xsl:if>
		   	<xsl:if test="(($diffX = 0) and ($diffY &gt; 0))">
			<div id="id3" style="position:absolute; left:{($x2 - 6)}px; top:{$y2}px; z-index:2; visibility: visible">
				<table><tr><td><img src="a_down.png"></img></td></tr></table>
			</div>
			</xsl:if>
		</xsl:when>
		</xsl:choose>

	<xsl:choose>
	<xsl:when test="$xPos = 1">
	<div id="id2" style="position:absolute; left:{$xLoc}px; top:{$yLoc}px; z-index:2; visibility: visible">
	<table width="{$xPos}" height="{$yPos}" cellspacing="0" cellpadding="0">
	<tr>
	<td width="{$xPos}" height="{$yPos}" style="border-left: 1px solid #000000"><img src="spacer.gif"></img></td>
	</tr>
	</table>
	</div>
	</xsl:when>

	<xsl:when test="$yPos = 1">
	<div id="id2" style="position:absolute; left:{$xLoc}px; top:{$yLoc}px; z-index:2; visibility: visible">
	<table height="{$yPos}" cellSpacing="0" cellPadding="0" width="{$xPos}">
	<tr>
	<td width="{$xPos}" height="{$yPos}" style="border-top: #000000 1px solid"><img src="spacer.gif"></img></td>
	</tr>
	</table>
	</div>
	</xsl:when>
	</xsl:choose>

</xsl:template>		


</xsl:stylesheet>
