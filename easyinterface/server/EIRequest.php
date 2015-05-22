<?php

class EIRequest 
{

  // The request is done as POST request, which consists of a json
  // object.
  //
  // This object includes one filled call 'command', and the other
  // fields depend on the value of this command
  
  private $request;
  private $config_dir;

  function __construct() {
    if (eregi('(Mac_PowerPC)|(Macintosh)', $_SERVER['HTTP_USER_AGENT'])){
      // for MAC   
      $this->request = json_decode(urldecode(stripslashes($_POST['eirequest'])),true);
    }else{
      $this->request = json_decode(urldecode($_POST['eirequest']),true);
    }
    if ( ! array_key_exists( 'command', $this->request ) )
      throw new Exception("Missing command in EI request "); 
  }

   public function process()
   {

     switch ( $this->request['command'] ) {
     case "general_info":
       return file_get_contents(EIConfig::$cfgDir . "/" . EIConfig::$cfgFile  );
       break;
       
     case "app_info":
       if ( ! array_key_exists( 'app_id', $this->request ) )
	 throw new Exception("Missing app_id");

       return EIApps::get_app_info(  $this->request['app_id']  );

     case "app_parameters":
       if ( ! array_key_exists( 'app_id', $this->request ) )
	 throw new Exception("Missing app_id");

       return EIApps::get_app_parameters(  $this->request['app_id']  );

     case "app_details":
       if ( ! array_key_exists( 'app_id', $this->request ) )
	 throw new Exception("Missing app_id");

       return EIApps::get_app_details(  $this->request['app_id']  );

      case "exset_details":
       if ( ! array_key_exists( 'exset_id', $this->request ) )
	 throw new Exception("Missing exset_id");

       return EIExamples::get_exset_details(  $this->request['exset_id']  );

    case "execute":
       if ( ! array_key_exists( 'app_id', $this->request ) )
	 throw new Exception("Missing app_id");

       if ( ! array_key_exists( 'parameters', $this->request ) )
	 throw new Exception("Missing parameters");

       return EIApps::execute(  $this->request['app_id'], $this->request['parameters'] );

     default:
       throw new Exception("Invalid command in EI request");
     }
   }
}

?>
