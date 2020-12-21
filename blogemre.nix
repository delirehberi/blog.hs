{mkDerivation,stdenv,base,scotty,wai,wai-extra,directory,text, filepath,pandoc,aeson,bytestring}:
mkDerivation {
  pname = "blogemre";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base scotty wai wai-extra directory text filepath pandoc aeson bytestring
  ];
  homepage = "http://emre.xyz/come";
  description = "demo project";
  license = with stdenv.lib.licenses; [gpl3Plus];
}
