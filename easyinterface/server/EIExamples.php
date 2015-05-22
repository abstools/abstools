<?php

class EIExamples {

  private static function expand_exset_ids($exset_id) {
   if ( gettype( $exset_id ) == "array" ) {
      $exset_ids = $exset_id;
    } else if ( $exset_id == "_ei_all" ) {
      $exset_ids = EIConfig::get_exsetIds();
    } else {
      $exset_ids =  array( $exset_id );
    }

   return $exset_ids;
  }


  static function get_exset_details( $exset_id ) {
    $exset_ids = EIExamples::expand_exset_ids($exset_id);
    $out = '<examples>';
    foreach ($exset_ids as $id ) {
      $exset = EIConfig::get_exsetXML( $id );
      /*if ( $exset->info ) 
	$info = $exset->info->asXML();
      else
	$info = "<info></info>";

      if ( $exset->content ) 
	$content = $exset->content->asXML();
      else
	$content = "<content></content>";
      */
      $out .= $exset->asXML();//'<exset id=\'' . $id . '\'>' . $info . $content . '</exset>';
    }
    $out .= '</examples>';

    return $out;
  }
  
}

?>
