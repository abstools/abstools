<?php

class EIConfig {
  static public $cfgDir            = "./config";
  static public $cfgFile           = "./eiserver.cfg";

  static private $cfgXML            = null;

  static private $appsXML          = null;
  static private $appIds           = null;
  static private $appXML           = null;
  static private $appInfoXML       = null;
  static private $appHelpXML       = null;
  static private $appExecXML       = null;
  static private $appParametersXML = null;

  static private $examplesXML       = null;
  static private $exsetIds          = null;
  static private $exsetXML          = null;


  static function init() {
    EIConfig::$appXML = array();
    EIConfig::$appInfoXML = array();
    EIConfig::$appHelpXML = array();
    EIConfig::$appExecXML = array();
    EIConfig::$appParametersXML = array();
    EIConfig::$exsetXML = array();
  }



  static private function expand_cfg_fname( $fname ) {
    return EIConfig::$cfgDir . "/" . $fname;
  }

  static private function expand_xml( $xml ) {
    if ( $xml->attributes()->src ) {
      $fname = EIConfig::expand_cfg_fname( $xml->attributes()->src );
      if ( file_exists ( $fname ) ) {
	$expXML = simplexml_load_file( $fname );
      } else {
	throw new Exception("File not found: " . $xml->attributes()->src );
      }
    } else if ( $xml->attributes()->cmd ) {
      $output = array();
      exec( $xml->attributes()->cmd, $output);
      $expXML = simplexml_load_string( implode("\n", $output) );
    } else {
      $expXML = $xml;
    }

    return $expXML;
  }


  /* load_cfgXML(): 

       Loads the XML structure of the main configuration. Note that
       reference to other external parts using the 'src' attribute are
       not resolved.
  */
  static private function load_cfgXML() {
    if ( EIConfig::$cfgXML == null ) {
      $fname = EIConfig::expand_cfg_fname( EIConfig::$cfgFile );
       if ( file_exists ( $fname ) ) {
	EIConfig::$cfgXML = simplexml_load_file( $fname );
      } else {
	throw new Exception("Server configuration file not found.");
      }
    }
  }

  /* load_appsXML: 

       Loads the XML structure of the apps configuration. Note that
       reference to other external parts using the 'src' attribute are
       not resolved.
  */
  static private function load_appsXML() {
    EIConfig::load_cfgXML();
    if (  EIConfig::$cfgXML->apps )
      EIConfig::$appsXML = EIConfig::expand_xml( EIConfig::$cfgXML->apps );
    else
      EIConfig::$appsXML = simplexml_load_string("<apps></apps>");
  }


  /* load_appXML: 

       Loads the XML structure of a app configuration. Note that
       reference to other external parts using the 'src' attribute are
       not resolved.
  */
  static private function load_appXML( $id ) {
    if ( ! array_key_exists(  $id, EIConfig::$appXML ) ) {
      EIConfig::load_appsXML();
      $appXML = EIConfig::$appsXML->xpath("//app[@id='" . $id . "']");
      if ( array_key_exists( 0, $appXML ) )
	$appXML = $appXML[0];
      else
	throw new Exception("Cannot find the app '" . $id . "'" );
      EIConfig::$appXML[$id] = EIConfig::expand_xml( $appXML );
    }
  }
  
  /* get_appVisible(id)
   */
  static function get_appVisible($id){
    EIConfig::load_appXML($id);
    $visib = "true";
    if(EIConfig::$appXML[$id]->attributes()->visible)
      $visib = EIConfig::$appXML[$id]->attributes()->visible;
    return $visib;
  }

  /* get_appIds()
   */
  static function get_appIds() {
    if ( EIConfig::$appIds == null ) {
      EIConfig::load_appsXML();
      $ids = EIConfig::$appsXML->xpath("/apps/app/@id");
      $appIds = array();
      $i=0;
      foreach ( $ids as $id ) {
	$appIds[$i] = (string) $id;
	$i++;
      }
      EIConfig::$appIds = $appIds;
    }
    return EIConfig::$appIds;
  }


  /* get_appInfoXML( $id )
   */
  static function get_appInfoXML( $id ) {
    if ( ! array_key_exists(  $id, EIConfig::$appInfoXML ) ) {
      EIConfig::load_appXML( $id );
      if (  EIConfig::$appXML[$id]->appinfo ) {
	EIConfig::$appInfoXML[$id] = EIConfig::expand_xml( EIConfig::$appXML[$id]->appinfo );
      } else {
	EIConfig::$appInfoXML[$id] = simplexml_load_string("<appinfo></appinfo>");
      }
    }

    return EIConfig::$appInfoXML[$id];
  }


  /* get_appHelpXML( $id )
   */
  static function get_appHelpXML( $id ) {
    if ( ! array_key_exists(  $id, EIConfig::$appHelpXML ) ) {
      EIConfig::load_appXML( $id );
      if (  EIConfig::$appXML[$id]->apphelp ) {
	EIConfig::$appHelpXML[$id] = EIConfig::expand_xml( EIConfig::$appXML[$id]->apphelp );
      } else {
	EIConfig::$appHelpXML[$id] = simplexml_load_string("<apphelp></apphelp>");
      }
    }

    return EIConfig::$appHelpXML[$id];
  }


  /* get_appExecXML( $id )
   */
  static function get_appExecXML( $id ) {
    if ( ! array_key_exists(  $id, EIConfig::$appExecXML ) ) {
      EIConfig::load_appXML( $id );
      if ( EIConfig::$appXML[$id]->execinfo ) {
	EIConfig::$appExecXML[$id] = EIConfig::expand_xml( EIConfig::$appXML[$id]->execinfo );
      } else {
	throw new Exception("App '" . $id . "' does not have an 'execinfo' environment (you must provide one)");
      }
    }

    return EIConfig::$appExecXML[$id];
  }

  /* get_appParametersXML( $id )
   */
  static function get_appParametersXML( $id ) {
    if ( ! array_key_exists(  $id, EIConfig::$appParametersXML ) ) {
      EIConfig::load_appXML( $id );
      if ( EIConfig::$appXML[$id]->parameters ) {
	EIConfig::$appParametersXML[$id] = EIConfig::expand_xml( EIConfig::$appXML[$id]->parameters );
      } else {
	EIConfig::$appParametersXML[$id] = simplexml_load_string("<parameters></parameters>");
      }
    }
    return EIConfig::$appParametersXML[$id];
  }

  /* get_appParametersARRAY
     converts xml format of appParameters to php array format
  */
  static function get_appParametersARRAY( $id ){
    $tmpxml = EIConfig::get_appParametersXML($id);
    return  EIConfig::xml2array($tmpxml);
  }
  
  /* load_examplesXML: 

       Loads the XML structure of the examples configuration. Note that
       reference to other external parts using the 'src' attribute are
       not resolved.
  */
  static private function load_examplesXML() {
    EIConfig::load_cfgXML();
    if ( EIConfig::$cfgXML->examples )
      EIConfig::$examplesXML = EIConfig::expand_xml( EIConfig::$cfgXML->examples );
    else 
      EIConfig::$examplesXML = simplexml_load_string("<examples></examples>");
  }


  /* get_exsetIds()
   */
  static function get_exsetIds() {
    if ( EIConfig::$exsetIds == null ) {
      EIConfig::load_examplesXML();

      $ids = EIConfig::$examplesXML->xpath("/examples/exset/@id");
      $exsetIds = array();
      $i=0;
      foreach ( $ids as $id ) {
	$exsetIds[$i] = (string) $id;
	$i++;
      }
      EIConfig::$exsetIds = $exsetIds;
    }

    return EIConfig::$exsetIds;
  }


  /* load_exsetXML: 

       Loads the XML structure of an exset configuration.
  */
  static private function load_exsetXML( $id ) {

    if ( ! array_key_exists(  $id, EIConfig::$appXML ) ) {
      
      EIConfig::load_examplesXML();

      $exsetXML = EIConfig::$examplesXML->xpath("//exset[@id='" . $id . "']");

      if ( array_key_exists( 0, $exsetXML ) )
	$exsetXML = $exsetXML[0];
      else
	throw new Exception("Cannot find the exset '" . $id . "'" );

      EIConfig::$exsetXML[$id] = EIConfig::expand_xml( $exsetXML );
    }

  }

  /* get_exsetXML($id): 

  */
  static public function get_exsetXML( $id ) {
    EIConfig::load_exsetXML($id);
    return EIConfig::$exsetXML[$id];
  }
  
  /* this function convert an xml to array
     with all attributes and childs */
  static function xml2array($xmlObject, $out = array())
  {
    foreach($xmlObject->attributes() as $attr => $val)
       $out['@attr'][$attr] = (string)$val;
       $has_childs = false;
    foreach($xmlObject as $index => $node)
      {
        $has_childs = true;
        $out[$index][] = EIConfig::xml2array($node);
	}
    if (!$has_childs && $val = (string)$xmlObject)
      $out['@value'] = $val;

     foreach ($out as $key => $vals)
      {
	if (is_array($vals) && count($vals) === 1 && array_key_exists(0, $vals))
	  $out[$key] = $vals[0];
	  }
    return $out;
    }

}

EIConfig::init();

?>
