{mkDerivation,stdenv,base}:
mkDerivation {
  pname = "come";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
  ];
  homepage = "http://emre.xyz/come";
  description = "demo project";
  license = with stdenv.lib.licenses; [gpl3Plus];
}
