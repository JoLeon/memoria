<?php
	require("fbSDK/autoload.php");
	use Facebook\FacebookSession;
	use Facebook\FacebookRequest;
	use Facebook\GraphUser;
	use Facebook\FacebookRequestException;
	use Facebook\FacebookRedirectLoginHelper;
	$facebook = FacebookSession::setDefaultApplication('167654036746736', 'a279b8844b8e3debac744125c64c03fe');
	$session = FacebookSession::newAppSession();
	$sql = "SELECT
				users.*
			FROM
				users
			WHERE
				users.id != 0
				AND users.id != 8
				AND users.tipo = 1
				AND users.estado = 1
				AND users.uid IS NOT NULL";
	$db = mysql_connect("localhost","root","1234");
	$selected = mysql_select_db("mem_playgue");
	$rs = mysql_query($sql);
	$getFromFacebook = array();
	while($row = mysql_fetch_assoc($rs)){
		if(!empty($row["uid"])){
			$tmp = new StdClass();
			$tmp->id = $row["id"];
			$tmp->fbid = $row["uid"];
			array_push($getFromFacebook, $tmp);
		}
	}
	mysql_close();
	$total = sizeof($getFromFacebook);
	$current = 1;
	$updateQuery = "";
	foreach($getFromFacebook as $user){
		print_r("Fetching... ".$current."/".$total."\n");
		print_r("/".$user->fbid."\n");
		$request = new FacebookRequest($session, 'GET', '/'.$user->fbid);
		try{
			$response = $request->execute();
			$graphObject = $response->getGraphObject();
			print_r($graphObject->getProperty("first_name")." ".$graphObject->getProperty("last_name").": ".$graphObject->getProperty('gender')."\n");
			$gender = $graphObject->getProperty('gender');
			$gender = strtoupper(substr($gender,0,1));
			$updateQuery .= "UPDATE users SET genero = '$gender' WHERE id = '$user->id';";
			print_r($graphObject);
		}
		catch (Exception $e){
			print_r("Error: perfil borrado"."\n");
		}
		$current++;
	}
	$file = "sqlQuery.sql";
	file_put_contents($file, $updateQuery);
	//$selected = mysql_select_db("mem_playgue", $db);
	//$users = mysql_query($sql);
	//while( $r = mysql_fetch_array($users)){
		//print_r("SOP");
	//}
?>
