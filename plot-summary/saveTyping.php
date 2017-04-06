<?php
$myFile = "data/".$_SERVER["REMOTE_ADDR"].".txt";
$fh = fopen($myFile, 'a');
fwrite($fh,$_POST["dataBlob"]."\n");
fclose($fh);


//$uIP = $_SERVER["REMOTE_ADDR"];
//$dt = date("YYYY-MM-DD HH-MM-SS");
//
//mysql_connect('entropic1.db.4789690.hostedresource.com','entropic1','RodesRodes22');
//mysql_select_db('entropic1');
//
//mysql_query("insert into negCv1 set dt=now(), uIP='".$uIP."', sdata='".$_POST["sdata"]."',turknum='".$_POST["turknum"]."';");
//
//#mysql_query("insert into speak set dt='".$dt."', uIP='".$uIP."', sqdata='".$_POST["sdata"]."', img='".$_POST["img"]."';");
//
//mysql_close();
//
//echo mysql_error();

?>
