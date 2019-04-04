"C:\Program Files\PeaZip\res\7z\7z.exe" a -sfx uni_logger_pack.exe logger.ico README.md settings.ini uni_logger.exe uni_logger_single.exe
move /Y uni_logger_pack.exe ".\packages\uni_logger_pack.exe"
"C:\Program Files\NSIS\Bin\makensis.exe" uni_logger_install.nsi
