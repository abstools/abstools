<?php

class EIApps {
  private function println($s) {
    printf("%s\n",$s);
  }

  private static function expand_app_ids($app_id) {
    if ( gettype( $app_id ) == "array" ) {
      $app_ids = $app_id;
    } else if ( $app_id == "_ei_all" ) {
      $app_ids = EIConfig::get_appIds();
    } else {
      $app_ids =  array( $app_id );
    }

    return $app_ids;
  }


  static function get_app_info( $app_id ) {
      $app_ids = EIApps::expand_app_ids($app_id);
      $out = '<apps>';
    foreach ($app_ids as $id ) {
      if(EIConfig::get_appVisible( $id )== "true"){
	$out .= '<app id=\'' . $id . '\'>';
	$out .= EIConfig::get_appInfoXML( $id )->asXML();
	$out .= '</app>';
      }
    }
     $out .= '</apps>';

    return $out;
  }

static function get_app_help( $app_id ) {
      $app_ids = EIApps::expand_app_ids($app_id);
      $out = '<apps>';
    foreach ($app_ids as $id ) {
      if(EIConfig::get_appVisible( $id )== "true"){
	$out .= '<app id=\'' . $id . '\'>';
	$out .= EIConfig::get_apphelpXML( $id )->asXML();
	$out .= '</app>';
      }
    }
     $out .= '</apps>';

    return $out;
  }

  static function get_app_parameters( $app_id ) {
    $app_ids = EIApps::expand_app_ids($app_id);
    $out = ' <apps> ';
    foreach ($app_ids as $id ) {
      if(EIConfig::get_appVisible( $id )== "true"){
	$out .= '<app id=\'' . $id . '\'>';
	$out .= EIConfig::get_appParametersXML( $id )->asXML();
	$out .= '</app>';
      }
    }
    $out .= ' </apps> '; 

    return $out;
  }

  static function get_app_details( $app_id ) {
    $app_ids = EIApps::expand_app_ids($app_id);
    $out = '<apps>';
    foreach ( $app_ids as $id ) {
      if(EIConfig::get_appVisible( $id ) == "true"){
	$out .= '<app id=\'' . $id . '\'>';
	$out .= EIConfig::get_appInfoXML( $id )->asXML();
	$out .= EIConfig::get_appHelpXML( $id )->asXML();
	$out .= EIConfig::get_appParametersXML( $id )->asXML();
	$out .= '</app>';
      }
    }
    $out .= '</apps>'; 

    return $out;
  }


  static function execute( $app_id, $parameters ) {
    $execInfo = EIConfig::get_appExecXML($app_id);
    $output = "";

 //   switch ( $execInfo->attributes()->method ) {
 //   case "cmdline":
      $output = EIApps::execute_cmdline($app_id, $parameters);
 /*     break;
    case "server":
      $output = EIApps::execute_server($app_id, $parameters);
      break;
    default:
      throw new Exception("Invalid or missing 'method' in the execinfo "
			  ."environment of '" . $app_id . "'");
    }*/

    return $output;
  }

  static function execute_server( $app_id, $parameters ) {
    throw new Exception("Executing via server is not supported yet!");
  }

  static function execute_cmdline( $app_id, $parameters ) {
    $execInfo = EIConfig::get_appExecXML($app_id);
    $pAuxy = (EIConfig::get_appParametersARRAY($app_id));
    $paramsArr = $pAuxy["parameters"];

    // prefix to be attached to each parameter anme
    $param_prefix = "-";
    if( array_key_exists("prefix",$paramsArr["@attr"]))
	$param_prefix = $paramsArr["@attr"]["prefix"];

    // boolean to check compatible parameters or not
    $param_check = false;
    if( array_key_exists("check",$paramsArr["@attr"]))
	$param_check = $paramsArr["@attr"]["check"];

    // the parameters template
    if ( $execInfo->cmdlineapp ) {
      $cmdline =  (string) $execInfo->cmdlineapp;
    } else {
      throw new Exception("Could not find environment cmdlineapp for '" 
			  . $app_id . "'");
    }
    // session ID
    //
    $sessionid_str = "";
    
    // client ID
    //
    $clientid_str = "";
    if ( array_key_exists( '_ei_clientid', $parameters ) ) {    
      $clientid_str = $parameters['_ei_clientid'];
      unset( $parameters['_ei_clientid'] );
    }
    // files
    //   
    $files_str = "";
    $dirs_str = "";
    $root_str = "";
    if ( array_key_exists( '_ei_files', $parameters ) ) {    
      $aux = tempnam(sys_get_temp_dir(),"_ei_files");
      $dir = $aux;
      unlink($aux);
      mkdir($dir, 0755);
      $root_str = $dir;
      EIApps::build_directories($files_str,$dirs_str,$dir,$parameters,true);
      unset( $parameters['_ei_files'] );
    }

    // outline
    //
    $outline_str = "";
    if ( array_key_exists('_ei_outline',$parameters) ) {
      $outline_str = implode(" ", $parameters['_ei_outline'] );
      unset( $parameters['_ei_outline'] );
    }

    $typesofparams = array("selectone","selectmany","flag","hidden");
    // other parameters
    //
    $parameters_str = "";
    // first get what parameters we have to check
    // then check this.
    // finish write the rest if param_check is false
    $local;
    $localprefix;
    $localcheck;
    $localtype;
    $paramsdone = array();
    foreach ($parameters as $key => $values) {
      $encontrado = false;
      foreach ($paramsArr as $typep => $paramsgroup){
	//CUIDADO CUANDO SOLO HAY UNO DE UN TIPO AQUI ESTA TODO....
	if(array_key_exists("@attr",$paramsgroup)){
	  //only exists one parameter of type "typep"
	  //paramsgroup = parameter
	  $actual = $paramsgroup;
	  if(!array_key_exists("name",$actual["@attr"])){
	    throw new Exception("Missing param name in the app config file");
	  }else if ($actual["@attr"]["name"]==$key){
	    //found parameter
	    $local = $actual; //copy to check after. 
	    $localtype = $typep;
	    $encontrado = true; 
	    break;
	  }
	}else if($typep != "@attr"){
	  //exists more than one parameter of type "typep"
	  //paramsgroup[N] = [N]parameter
	  foreach($paramsgroup as $k=>$actual){
	    if(!array_key_exists("name",$actual["@attr"])){
	      throw new Exception("Missing param name in the app config file");
	    }else if ($actual["@attr"]["name"]==$key){
	      $local = $actual;//copy to check after
	      $localtype = $typep;
	      $encontrado = true;
	      break;
	    }
	  }//end foreach paramsgroup
	  if($encontrado)
	    break;
	}
      }//end foreach paramsArr*/
      if($encontrado){
	//THE PARAMETER IS IN THE VARIABLE local
	$localprefix = $param_prefix;
	if(array_key_exists("prefix",$local["@attr"])){
	  $localprefix = $local["@attr"]["prefix"];
	}
	$localcheck = $param_check;
	if(array_key_exists("check",$local["@attr"])){
	  $localcheck = $local["@attr"]["check"];
	}
	if($localcheck=="true"){//check this parameter 
	  switch( $localtype ){
	  case "selectone":
	    if( count($values)!=1)
	      throw new Exception("This parameter (".$local["@attr"]["name"]
				  .") only accept one value");
	    if(!EIApps::checkvalue($local,$values[0]))
	      throw new Exception("Invalid parameter's value: ".$k
				  ."in parameter: ".$local["@attr"]["name"] );
	    break;

	  case "selectmany":
	    if(count($values)<1)
	      throw new Exception("This parameter (".$local["@attr"]["name"].
				  ") requeire at least one value");
	    foreach ($values as $v)
	      if(!EIApps::checkvalue($local,$v))
		throw new Exception("Invalid parameter's value: ".$v
				    ." in parameter: ".$local["@attr"]["name"] );
	    break;

	  case "flag":
	    if( count($values)>1)
	      throw new Exception("This parameter (".$local["@attr"]["name"]
				  .") only accept one value");
	    else if(count($values)==0) $values[0]="no";
	    if($values[0] != "yes" && $values[0] != "no")
	      throw new Exception("Flag parameter only accept true or false");
	    break;

	  case "hidden":
	    break;

	  case "textfield":
	    break;

	  default: 
	    throw new Exception("Unknown type param: ".$localtype);
	    break;

	  }//end switch localtype check
	}//end localcheck
      }else{// end encontrado
	//this parameter is not specificated on the config file
        throw new Exception("This app doesnt accept more parameters".
			      " than the especificated");
      }

      //$key is the name of the param in the XML
      //$values is an array with all the values
     
      switch ($localtype){
      case "selectone": 
	$parameters_str .= " ".$localprefix."".$key ;
	$parameters_str .= " ".$values[0];
	break;
      case "selectmany":
	$parameters_str .= " ".$localprefix."".$key ;
	foreach($values as $val)
	  $parameters_str .= " ".$val;
	break;
      case "flag":
	if($values[0] == "yes")
	  $parameters_str .= " ".$localprefix."".$key ;
	break;
      case "hidden":
	$parameters_str .= " ".$localprefix."".$key ;
	$parameters_str .= " ".$values[0];
	break;
      case "textfield":
	$parameters_str .= " ".$localprefix."".$key;
	$parameters_str .= " '".$values[0]."'";
	break;
      }//end switch localtype write str
    }//end parameters
    $replace_pairs = array(
			   "_ei_files" => $files_str,
			   "_ei_dirs" => $dirs_str,
			   "_ei_root" => $root_str,
			   "_ei_outline" => $outline_str,
			   "_ei_parameters" => $parameters_str,
			   "_ei_clientid" => $clientid_str,
			   "_ei_sessionid" => $sessionid_str
			   );
    
    $cmdline = strtr( $cmdline, $replace_pairs);
    $cmdline = escapeshellcmd($cmdline);
 
   
    //    print $cmdline; // TODO -- shoudl go into some tags
    $outputLines = array();
    exec($cmdline, $outputLines);
    $output =  implode("\n", $outputLines);

    return $output;
  }


  private static function checkvalue($posibles, $actual){
    foreach($posibles["option"] as $opt){
      if(!array_key_exists("value",$opt["@attr"]))
	throw new Exception("Mising value of attribute in the configuration file.");
      if($opt["@attr"]["value"] == $actual)
	return true;
    }
    return false;
  }

  private static function build_directories(& $files_str,& $dirs_str,
					    $dir, $parameters,$recursive)
  {
    foreach ( $parameters['_ei_files'] as $file ) {
      if(!$recursive && !preg_match("/^[a-zA-z0-9\@\-_\.]+$/",$file["name"])) 
	throw new Exception("Forbidden filename: ".$file["name"]
			    .". Filenames can only contain the "
			    ."following characters: [a-z][A-Z][0-9][_][-][.]."
			    ." Spaces and others characters are forbbiden.");
      if($recursive && !preg_match("/^[a-zA-z0-9\@\-_\.\/]+$/",$file["name"])) 
	throw new Exception("Forbidden filename: ".$file["name"]
			    .". Filenames can only contain the "
			    ."following characters: [a-z][A-Z][0-9][_][-][.]."
			    ." Spaces and others characters are forbbiden.");
      if( array_key_exists('type',$file)
	  && strcmp($file['type'],'directory')==0){
	// is dir then create dir and save path on dirs_str
	$newdir=$dir."/".$file["name"];
	mkdir($newdir,0755,$recursive);
	$dirs_str .= " ".$newdir;
	//search more files or dirs on this newdir
	EIApps::build_directories($files_str,$dirs_str,$newdir,$file,false);
      }
      else{ //is file then save name and content
	$aux = $file["name"];
	$filename = $dir."/".$aux;
	file_put_contents($filename,$file["content"]);
	$files_str .= " ".$filename;
      }
    }
  }	  
}

?>
