** -*- koshu -*-
**
**  常用漢字の /読み を /音読み と /訓読み に分割します．
**

input  "../joyo-kanji.k"
output "on-kun.k"

|== 音訓 : 音訓
  --order --forward /通用字体 /音読み /訓読み
  
音訓 : 読み
  | hier /通用字体 -to /g
  | slice /音 ( ^/g | keep /カタカナ )
  | slice /訓 ( ^/g | omit /カタカナ )
  | add /音読み ( to-set of sort /音/読み個別 )
  | add /訓読み ( to-set of sort /訓/読み個別 )
  | cut /g /音 /訓
  
読み : source 常用漢字 /通用字体 /読み
  | elem /読み -to /読み個別
  | cut /読み
  | add /1 ( take 1 of trim-text-both "（）()" /読み個別 )
  | add /カタカナ ( /1 >= "゠" and /1 <= "ヿ" )
  | cut /1
