<?php
header('Content-type:application/json');

if (isset($_SERVER['HTTP_ORIGIN'])) {
    header("Access-Control-Allow-Origin: {$_SERVER['HTTP_ORIGIN']}");
}

require('accounts.php');

function getFrom($arr){
    if(count($arr)){
        $from = $arr[0];
        return $from->mailbox . "@" . $from->host;
    } else {
        return '';
    }
}

function getTo($login,$arr){
    $dest = array();
    foreach($arr as $to)
        array_push($dest,$to->mailbox . "@" . $to->host);
    switch(count($dest)){
    case 0:
        return '';
    case 1:
        return $dest[0];
    default:
        if(in_array($login,$dest))
            return $login . ' + others';
        else
            return $dest[0] . ' + others';
    }
}

$errors = array();
$mails = array();

if(isset($_GET['p']) && $_GET['p'] == $master){
    foreach($accounts as $account){
        $imap = imap_open($account['srv'],$account['login'],$account['pass']);
        if($imap){
            if($search = imap_search($imap,"UNSEEN")){
                foreach($search as $num){
                    $headers = imap_headerinfo($imap,$num);
                    if($headers){
                        $date = new DateTime($headers->date);
                        $from = getFrom($headers->from);
                        $to = getTo($account['login'],$headers->to);
                        array_push($mails,array(
                            'subject'=>iconv_mime_decode($headers->subject,0,"UTF-8"),
                            'from'=>imap_utf8($from),
                            'to'=>imap_utf8($to),
                            'date'=>$date->format("d/m/Y H:i"),
                            'udate'=>$headers->udate,
                            'webmail'=>$account["webmail"]));

                    } else {
                        array_push($errors,"imap_headerinfo failure");
                    }
                }
            }
        } else {
            array_push($errors,"Connection to " . $account['srv'] . " server failed");
        }
    }

} else {
    array_push($errors,"Authentification Failed");
}

echo json_encode(array("errors"=>$errors,"mails"=>$mails));
?>
