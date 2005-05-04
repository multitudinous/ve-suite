setenv PATH /home/vr/Applications/TSVEG/VE_Suite/bin:${PATH}
VES.vrac -nserv

pushd /home/vr/Applications/TSVEG/Test_Pit/Deere_Harvester/
VES.vrac -sim
popd
