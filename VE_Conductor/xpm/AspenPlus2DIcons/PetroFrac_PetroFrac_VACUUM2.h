#ifndef GETVESUITE_PetroFrac_PetroFrac_VACUUM2_H
#define GETVESUITE_PetroFrac_PetroFrac_VACUUM2_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_PetroFrac_PetroFrac_VACUUM2( void )
{
    unsigned char osgData[ 9334 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,135,0,69,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,254,40,175,0,253,161,63,105,223,132,95,178,254,143,224,61,103,226,221,223,196,15,248,186,31,16,23,225,111,195,223,15,252,45,248,39,241,179,246,128,241,223,140,60,119,255,0,8,39,142,254,39,77,161,104,95,14,62,0,252,61,241,63,136,47,124,143,135,255,0,12,188,121,171,93,93,46,153,246,59,75,63,12,220,73,115,113,23,238,195,252,255,0,255,0,15,44,253,157,127,232,156,254,223,255,0,248,169,239,248,42,111,255,0,65,189,0,125,255,0,95,152,63,240,81,127,248,42,231,236,239,255,0,4,231,211,188,57,160,248,218,211,94,248,161,241,207,226,14,145,171,234,191,15,62,6,248,6,125,36,120,138,227,79,181,178,214,97,209,252,95,241,11,86,212,174,214,63,135,127,12,239,60,93,165,197,163,38,168,214,250,142,165,115,59,95,205,162,104,58,228,90,22,188,52,223,31,253,168,63,224,171,30,1,248,71,225,84,253,161,124,21,225,239,219,126,127,4,124,16,240,255,0,141,60,89,241,195,225,15,141,191,224,150,223,240,80,191,2,120,87,226,55,194,91,77,58,199,92,241,111,137,116,63,140,30,59,253,143,244,219,79,132,255,0,24,60,39,164,248,106,255,0,83,240,221,239,136,117,253,59,225,222,175,109,125,174,120,103,199,50,120,122,45,107,71,248,161,240,207,248,241,177,248,181,7,237,239,251,101,124,116,253,175,47,100,184,215,124,59,241,183,226,94,177,227,95,0,143,22,233,58,111,135,188,71,99,240,126,197,45,188,51,240,63,195,254,33,208,124,48,141,167,218,120,131,73,248,71,164,120,27,76,188,49,205,118,243,205,163,205,61,221,254,163,115,44,247,183,92,216,186,239,15,70,83,74,242,232,56,171,180,126,132,124,64,253,189,255,0,224,169,63,182,228,62,22,127,16,124,102,151,246,110,240,157,130,233,55,146,248,95,246,78,30,41,248,63,253,175,172,193,111,226,43,97,174,234,222,52,151,198,58,151,139,46,55,167,136,224,142,231,74,62,34,26,4,208,105,90,93,234,232,199,82,183,55,178,121,151,136,191,98,15,137,31,29,110,108,252,93,241,223,226,31,197,239,142,94,51,211,236,71,135,45,60,93,241,107,199,94,40,248,135,226,11,15,15,218,94,94,106,86,94,30,211,181,127,23,106,151,55,86,58,12,55,186,190,165,113,29,160,144,68,151,58,173,221,194,40,55,14,79,233,223,236,235,240,183,73,149,109,37,146,222,213,158,69,69,193,102,120,212,99,1,4,126,64,204,96,179,113,156,242,114,119,18,79,236,31,195,127,217,223,195,151,150,49,221,220,218,33,153,44,132,114,137,154,17,18,144,99,126,4,101,185,1,2,238,224,112,78,112,126,95,142,197,102,245,212,157,231,167,151,95,248,99,166,52,211,90,105,253,35,249,72,159,224,183,237,113,240,87,194,235,224,127,131,31,181,103,237,85,240,143,192,222,30,179,186,95,13,120,7,192,127,29,62,39,120,67,193,122,13,254,179,121,121,171,94,190,139,225,175,14,248,158,214,199,76,130,231,91,212,175,47,174,146,56,195,205,117,123,119,116,251,228,152,147,246,175,193,47,248,47,151,237,93,251,53,248,163,66,240,239,237,221,224,157,43,227,175,193,235,77,27,197,214,250,199,196,223,133,30,8,209,124,45,251,65,205,226,9,111,164,213,252,41,172,94,232,203,226,221,23,193,58,246,139,20,101,180,121,52,235,93,55,195,51,165,133,245,142,177,46,167,119,121,97,119,107,174,126,178,124,105,248,47,225,141,10,209,225,131,236,190,90,71,112,135,207,158,220,159,48,237,37,163,101,144,176,59,72,82,118,227,106,183,221,46,0,252,33,253,171,190,26,120,106,109,35,92,180,149,52,73,33,120,110,100,140,222,221,226,43,89,150,57,74,205,189,98,6,36,4,157,220,145,140,228,16,206,15,78,11,54,172,229,24,202,77,173,55,249,19,42,105,90,251,159,218,15,193,143,140,255,0,11,127,104,127,133,190,10,248,213,240,87,198,186,55,196,63,133,255,0,16,244,100,215,60,37,226,221,13,231,251,38,163,103,231,207,101,121,107,117,103,123,4,55,90,38,187,99,170,218,95,216,106,154,93,252,22,218,150,147,169,105,151,122,102,167,105,105,127,105,115,109,17,95,193,207,252,18,199,254,11,85,251,63,255,0,193,37,161,253,163,62,17,126,211,13,241,39,196,223,14,190,39,248,219,194,63,18,254,19,248,111,225,22,143,240,171,88,111,15,120,222,61,19,84,240,183,197,253,111,196,90,175,143,190,32,120,110,249,191,180,244,29,23,224,204,22,118,209,221,106,86,144,127,194,41,117,44,80,105,243,92,220,201,168,149,245,180,234,42,144,140,215,218,71,59,209,181,216,254,153,63,106,143,218,153,126,33,254,208,191,179,198,169,240,147,225,63,143,62,33,120,95,246,10,253,171,252,127,241,31,227,23,137,33,254,204,209,45,252,75,119,109,251,52,254,210,223,178,175,138,124,21,240,202,202,250,119,159,87,215,116,141,71,227,159,140,47,103,155,89,93,6,198,234,247,225,149,182,157,97,115,115,165,248,129,60,75,165,125,57,251,0,254,219,31,19,255,0,110,27,79,138,223,18,174,63,103,13,51,225,151,236,193,14,167,161,159,217,75,246,131,210,126,56,232,191,19,45,127,106,47,13,77,172,248,251,66,241,206,165,55,195,232,252,15,162,106,127,9,245,79,11,235,94,19,210,52,235,230,145,245,223,13,120,130,247,89,184,191,240,15,138,188,97,224,248,52,191,22,235,191,136,22,86,191,17,244,237,47,194,31,179,182,153,240,251,92,248,165,251,10,252,102,241,255,0,139,60,3,251,46,107,94,22,130,211,84,139,254,10,50,154,69,191,136,188,85,224,159,4,248,163,196,215,118,246,35,74,253,154,35,248,111,224,155,157,127,94,150,91,61,3,195,159,27,244,207,12,107,26,246,153,170,95,126,206,218,70,177,115,241,71,250,3,253,132,126,1,248,167,224,39,193,109,74,223,226,4,17,233,255,0,18,254,42,252,64,241,31,197,239,136,58,5,158,167,101,170,233,94,22,214,117,251,29,19,195,90,31,134,52,235,171,24,154,63,180,217,248,15,194,30,16,143,86,17,222,106,150,167,94,26,180,154,118,167,119,165,189,139,47,22,26,182,50,120,138,208,175,77,70,17,218,219,37,211,95,180,222,254,75,205,180,170,74,54,86,122,158,159,241,183,224,103,252,47,125,99,225,70,153,226,207,20,253,159,224,255,0,195,255,0,136,30,21,248,197,226,143,134,250,118,137,229,107,63,18,190,41,252,32,241,223,129,254,39,254,206,247,58,159,196,1,171,137,252,55,224,15,11,124,77,240,133,183,138,111,244,125,54,194,29,71,196,186,239,133,252,45,111,123,226,27,95,7,217,120,187,194,30,59,255,0,42,191,216,83,246,120,241,23,139,60,39,224,175,22,120,63,226,143,196,136,53,9,188,23,225,187,185,126,29,106,255,0,24,190,50,120,59,225,230,165,4,122,70,145,53,214,149,162,235,63,8,252,105,163,107,62,0,213,111,110,224,142,40,245,103,111,18,105,186,76,90,141,212,171,225,45,80,199,101,111,109,254,186,181,254,84,159,240,79,47,136,62,12,248,111,240,159,225,182,183,227,111,16,88,104,54,55,126,17,240,182,143,164,197,63,157,113,171,248,147,196,23,90,37,157,206,157,225,79,8,232,22,17,203,127,227,31,24,222,165,157,202,233,218,54,149,109,121,170,106,50,197,228,88,218,92,78,201,25,211,27,41,194,146,112,87,149,246,181,250,118,8,43,182,159,245,177,251,45,240,87,192,126,13,240,108,118,145,124,127,248,71,255,0,5,22,240,165,172,97,45,155,199,159,179,207,237,159,255,0,5,8,253,166,190,27,234,250,197,209,23,186,110,139,225,173,27,224,15,198,179,241,50,38,58,40,186,123,205,75,92,248,97,225,237,2,207,80,210,46,180,229,214,46,26,227,65,184,215,127,160,63,217,227,246,55,253,142,126,54,252,54,211,126,36,252,40,253,160,63,107,63,138,31,15,252,67,111,169,255,0,194,63,227,175,134,31,240,86,255,0,248,40,167,138,188,19,175,54,143,170,223,232,90,184,209,252,71,224,223,219,30,109,63,85,251,46,185,165,234,118,87,34,25,164,16,221,233,211,219,74,22,104,100,69,252,174,248,7,241,215,227,127,139,227,176,143,224,7,236,191,175,107,182,131,101,221,167,143,63,105,79,26,73,251,47,124,59,214,244,251,98,182,122,157,134,147,161,143,3,248,191,226,118,153,226,232,245,185,4,80,90,248,131,225,134,131,164,223,89,233,87,250,157,174,188,246,205,161,141,123,244,199,225,71,252,19,27,194,94,52,248,143,226,223,218,59,246,129,248,175,226,111,16,124,116,241,231,134,34,208,60,107,175,254,202,122,5,151,236,17,105,168,91,233,48,248,115,71,240,234,91,252,93,253,157,181,72,62,62,248,139,195,241,248,91,194,30,28,134,251,195,62,53,248,237,227,63,7,94,234,118,201,170,199,225,219,47,236,127,7,89,120,83,228,49,53,90,77,214,174,240,178,190,138,50,231,191,116,227,171,93,245,146,237,110,221,16,233,104,243,127,75,250,208,248,187,246,201,248,75,251,17,124,14,241,12,30,3,241,15,198,79,219,147,91,248,151,125,162,15,21,67,240,155,192,63,183,87,252,21,71,246,134,248,195,23,130,46,239,181,13,30,223,226,46,161,240,127,225,127,199,191,19,120,151,76,248,96,117,237,38,243,76,147,196,215,90,84,58,4,122,171,65,165,77,168,174,161,119,105,109,55,224,223,237,17,240,59,199,190,36,211,245,153,60,53,240,243,227,239,193,45,25,108,239,33,185,213,191,104,223,219,215,246,149,248,155,227,171,27,219,88,77,236,247,22,31,9,62,19,254,210,254,35,208,188,85,225,171,187,83,107,101,13,213,223,196,141,26,254,222,234,91,219,171,141,18,123,91,11,56,181,207,232,47,91,253,146,190,48,126,195,62,18,212,124,13,251,39,73,240,15,226,151,194,237,127,91,212,188,75,173,124,58,248,159,240,235,225,31,236,123,226,123,15,28,106,186,126,147,165,234,158,56,178,248,151,251,17,254,205,54,30,16,215,180,88,252,63,225,63,12,216,69,225,251,255,0,131,235,226,11,137,239,238,181,27,191,137,77,166,233,154,87,133,227,252,131,253,167,255,0,105,93,2,223,76,215,109,126,45,252,58,248,175,251,60,95,77,101,127,42,201,241,119,195,186,29,215,130,224,211,174,32,104,52,237,71,89,248,211,240,151,197,30,42,240,15,133,228,188,213,150,226,194,215,78,213,188,87,99,173,77,122,182,241,13,48,46,169,163,62,163,213,133,171,52,227,245,121,253,101,117,147,122,252,161,163,95,62,98,100,149,215,55,187,253,119,254,190,244,127,27,63,182,79,129,19,225,255,0,139,244,93,36,120,199,199,222,53,121,78,185,51,107,31,16,124,77,55,137,181,141,138,218,74,37,186,92,52,48,199,20,33,188,198,59,33,14,237,41,50,59,170,198,177,149,221,255,0,193,66,137,31,17,244,30,255,0,186,215,185,56,57,253,254,151,249,26,43,235,40,57,58,84,219,119,109,126,190,159,215,229,131,209,179,253,141,63,107,111,217,235,226,159,199,43,175,217,175,197,223,5,62,45,252,63,248,63,241,55,246,103,248,255,0,168,124,115,240,214,177,241,75,224,207,136,190,59,120,19,196,95,219,127,179,143,237,13,251,55,235,62,22,215,124,17,225,63,141,255,0,15,117,8,247,120,127,246,131,213,117,27,91,235,127,17,199,246,123,207,14,91,164,182,119,80,77,34,175,159,255,0,194,185,255,0,130,166,255,0,209,228,126,192,31,248,173,63,218,43,255,0,166,197,71,237,209,226,15,139,191,240,150,254,194,95,9,126,18,252,117,248,129,251,59,255,0,195,68,126,215,254,34,248,91,241,11,226,23,194,223,13,124,19,241,55,142,255,0,225,4,240,215,236,61,251,104,124,126,135,66,208,161,248,253,240,147,198,222,31,211,62,211,241,3,224,151,128,154,234,233,180,25,175,62,199,105,113,109,109,113,109,246,153,36,164,255,0,134,55,253,162,191,233,44,63,183,255,0,225,240,231,254,9,101,249,127,202,52,248,173,132,121,7,196,127,18,127,193,73,62,19,120,171,225,142,133,227,175,218,231,246,32,209,188,55,241,83,196,13,224,109,39,226,156,159,240,76,159,143,47,240,207,195,95,19,53,93,71,195,250,71,195,95,134,222,60,214,135,252,21,223,237,126,16,241,7,141,117,125,106,243,77,240,149,245,213,128,240,246,167,226,29,42,219,193,243,235,118,126,50,241,87,128,252,63,226,207,243,186,255,0,130,110,126,205,31,9,60,77,165,248,63,199,119,182,62,57,210,252,103,173,252,60,240,253,166,169,226,127,8,124,98,248,199,224,45,110,238,194,93,63,195,242,29,49,245,31,3,248,251,78,145,116,144,214,26,126,203,69,101,182,65,167,219,132,137,68,17,4,254,249,191,108,95,216,147,246,162,248,153,224,75,63,217,199,194,159,240,81,15,219,255,0,226,7,139,190,54,253,161,180,223,22,248,167,225,79,252,19,183,76,248,67,240,10,215,225,254,179,225,143,17,71,251,66,120,211,226,47,128,191,224,158,26,30,175,162,124,64,240,151,139,103,240,102,175,240,243,66,240,135,137,124,63,241,31,95,241,182,153,164,221,248,87,94,240,70,147,225,255,0,22,252,85,248,115,254,124,255,0,176,167,199,15,139,255,0,12,252,39,224,166,255,0,133,47,225,71,240,29,175,130,188,52,145,252,83,241,71,197,79,19,105,94,13,182,183,125,31,73,186,107,255,0,21,167,128,254,11,120,167,80,248,125,162,90,216,37,228,250,158,181,173,217,216,120,111,74,139,75,184,123,237,114,24,218,210,75,190,60,114,170,232,218,148,185,100,252,237,242,243,46,22,185,253,97,252,5,253,139,190,15,234,177,88,155,175,25,126,214,81,110,11,159,236,239,219,223,246,233,209,128,201,81,242,141,35,246,140,131,103,110,152,175,216,159,135,223,240,78,207,128,23,122,76,242,75,241,7,246,234,82,45,201,2,219,254,10,137,255,0,5,50,177,67,133,239,21,159,237,111,26,144,120,224,140,123,87,226,127,236,167,241,159,246,182,248,129,225,141,7,198,31,9,254,16,126,196,255,0,18,60,33,171,27,161,163,248,171,64,253,188,62,37,234,126,29,213,91,78,212,46,116,157,68,88,107,126,26,253,132,245,59,91,179,6,169,97,123,109,55,149,44,158,85,197,164,176,190,36,141,212,126,216,124,62,248,131,255,0,5,49,77,38,113,101,251,36,126,194,179,199,246,115,151,185,255,0,130,137,252,127,180,112,54,231,34,56,127,224,151,83,130,221,120,44,58,117,175,137,197,60,92,100,163,245,168,211,179,217,206,43,243,252,78,136,114,114,171,43,252,188,207,148,190,61,254,193,63,3,180,168,101,54,222,58,253,180,165,63,233,63,242,18,255,0,130,145,127,193,68,117,144,118,132,32,145,171,254,212,179,134,207,57,245,192,206,107,240,187,246,159,253,149,254,24,232,154,102,187,37,159,137,255,0,104,249,154,43,43,246,81,171,126,216,223,181,214,190,132,172,44,195,204,143,94,248,225,114,178,174,122,135,12,8,234,15,74,253,209,248,249,227,159,248,40,140,176,75,253,175,251,45,254,197,214,32,253,168,147,167,126,222,223,28,117,82,62,84,206,5,215,252,19,118,204,100,96,99,158,115,218,191,157,239,219,63,246,145,248,207,224,187,221,75,193,191,16,124,11,251,43,232,254,57,212,244,25,181,125,59,225,207,132,191,106,207,138,191,16,126,41,235,58,45,212,183,214,71,90,240,223,194,221,15,246,50,143,95,241,38,149,27,233,154,187,205,113,105,167,201,111,111,6,135,127,117,115,44,22,182,55,115,67,213,129,250,220,164,146,175,25,250,78,46,223,113,19,229,77,123,191,129,252,148,254,217,63,12,188,21,240,179,197,250,39,135,252,7,163,203,161,232,210,127,110,92,182,158,117,141,115,86,132,78,27,74,136,207,31,246,214,167,114,208,200,209,36,106,204,172,12,139,10,7,221,229,166,210,147,246,201,215,188,115,226,47,23,232,151,222,61,240,4,63,14,53,69,58,228,81,104,99,197,218,111,140,38,116,221,164,201,37,196,151,250,53,148,118,241,197,189,194,32,14,242,51,71,33,116,141,66,25,10,251,58,92,222,206,28,206,242,182,175,127,198,230,15,118,127,164,119,141,245,205,39,246,139,243,190,44,252,93,184,241,111,142,63,108,61,87,226,47,196,29,103,246,89,253,157,245,31,26,106,158,28,215,63,102,15,18,120,106,234,29,7,196,58,234,120,133,52,119,95,217,147,193,191,14,180,45,95,195,86,31,16,62,33,218,232,154,78,176,247,186,86,155,97,111,163,248,151,226,87,138,188,57,225,93,127,239,47,248,34,191,236,227,240,243,225,127,128,63,104,143,142,49,120,54,214,95,218,27,227,183,198,9,180,47,218,23,246,140,210,227,215,244,29,11,246,173,191,248,87,38,185,168,248,83,226,102,143,224,25,181,155,205,31,195,111,165,223,252,82,241,207,134,53,61,74,202,227,86,241,15,138,117,223,7,234,190,34,248,129,226,191,21,248,226,251,91,213,159,231,255,0,218,255,0,246,98,253,155,63,100,191,30,254,201,223,26,127,104,175,142,190,31,248,117,121,251,92,126,214,186,223,195,111,219,219,246,129,214,252,97,160,126,205,223,8,188,125,161,201,251,47,254,215,159,180,55,133,188,57,164,105,218,223,136,164,211,191,103,239,1,219,252,92,248,117,165,120,111,195,182,122,118,183,253,185,117,97,241,95,196,23,222,38,241,63,140,190,42,248,146,251,226,53,231,223,159,14,255,0,224,163,223,240,71,223,132,254,11,240,247,195,191,135,127,240,80,207,248,39,159,133,188,27,225,107,35,99,163,104,214,63,182,87,236,251,42,196,178,220,79,123,125,123,123,125,123,241,50,91,157,99,90,188,212,174,175,47,53,13,66,242,105,239,245,27,251,251,139,251,251,139,139,203,153,231,147,143,11,133,196,81,175,90,117,43,58,148,231,174,187,182,246,211,104,168,249,110,221,251,36,228,211,74,202,214,254,191,175,67,244,206,191,203,87,254,9,118,127,226,223,252,59,235,199,128,252,55,143,252,22,233,61,120,233,235,95,232,87,255,0,15,98,255,0,130,89,127,210,75,63,96,15,252,76,143,217,215,255,0,158,53,127,153,79,236,39,160,254,212,254,40,240,151,130,180,207,8,73,240,218,203,225,204,190,11,240,210,139,29,35,226,151,139,254,23,124,67,214,244,223,236,125,38,214,247,73,214,190,32,219,252,24,241,83,120,127,74,189,181,150,244,201,39,134,237,116,111,17,216,77,45,172,218,79,138,108,166,179,121,175,43,48,135,61,30,94,101,13,119,110,221,7,13,222,151,208,254,167,238,126,25,254,196,62,22,72,62,52,126,208,154,247,195,223,217,211,92,241,78,171,164,120,103,83,253,160,236,62,54,234,191,177,255,0,196,111,23,221,197,161,200,186,95,128,117,223,218,3,225,183,143,124,35,226,15,22,104,239,162,120,94,41,215,195,183,154,221,205,132,203,224,171,43,214,176,146,77,18,202,107,63,209,223,217,139,226,231,252,20,102,239,84,213,180,239,132,126,0,212,191,105,31,128,240,248,102,242,127,6,124,81,253,188,188,1,166,255,0,193,63,252,125,226,109,64,234,26,80,214,229,241,15,139,190,27,219,107,254,46,189,215,244,253,122,127,19,105,122,79,135,245,63,216,239,225,86,151,168,232,58,84,122,215,252,39,218,147,105,250,102,161,241,35,242,119,246,67,248,81,241,251,225,78,188,190,55,248,113,251,24,254,198,87,63,17,245,61,34,95,15,235,63,23,60,103,251,105,124,112,241,31,198,159,20,104,23,23,218,117,247,246,31,139,254,51,120,187,246,21,215,188,85,226,173,26,57,52,125,14,59,107,93,75,88,188,130,210,219,195,218,109,157,170,69,105,166,216,193,7,239,151,195,255,0,136,63,240,83,36,210,39,22,95,178,71,236,43,60,95,103,57,123,175,248,40,159,199,251,71,11,183,60,71,15,252,18,234,112,91,175,5,128,227,173,124,134,38,74,49,112,231,134,34,250,126,242,112,178,255,0,10,78,235,255,0,2,219,167,109,226,180,78,214,191,101,232,126,64,124,105,248,161,173,248,134,41,7,252,21,15,227,119,198,159,217,83,81,79,180,183,131,188,10,109,109,63,224,159,191,179,174,180,160,32,240,255,0,137,147,246,143,253,158,127,108,15,138,9,226,191,141,87,79,47,196,11,70,240,29,215,237,9,7,218,244,63,12,174,191,63,193,187,97,163,89,120,219,81,249,67,227,127,194,143,133,191,7,124,25,226,143,11,124,35,248,109,224,15,133,190,25,186,147,86,214,174,188,59,240,231,193,222,29,240,62,133,115,172,220,233,182,150,55,26,180,250,71,134,116,219,91,121,181,57,44,116,221,58,23,184,104,204,173,21,132,49,151,41,20,96,126,192,124,123,241,207,252,20,70,88,101,254,214,253,150,255,0,98,235,32,126,213,159,236,223,219,219,227,150,170,126,234,103,2,231,254,9,187,103,142,131,184,235,212,87,243,63,251,68,254,207,159,23,60,29,167,107,19,124,51,253,158,190,1,126,204,237,13,165,228,137,99,251,57,126,215,63,19,60,55,224,69,187,146,35,6,167,172,223,124,16,147,246,48,182,240,15,139,60,77,119,164,98,194,109,83,88,240,197,238,167,246,91,107,37,130,246,218,125,43,72,184,176,233,194,184,213,113,94,218,52,86,158,236,103,23,14,157,21,154,243,191,51,20,180,107,169,252,209,255,0,193,66,255,0,228,163,232,63,245,203,94,255,0,209,250,93,21,193,254,217,45,241,60,248,191,68,95,138,177,248,12,120,128,29,113,162,151,225,244,222,33,109,29,237,217,180,144,209,201,111,226,72,4,241,76,179,43,144,226,87,87,89,66,148,140,198,90,82,190,191,14,173,70,154,186,118,93,54,57,222,236,255,0,114,250,40,162,182,16,87,249,106,255,0,193,46,191,228,159,252,59,255,0,177,15,195,127,250,108,210,107,253,74,171,252,138,127,97,95,3,254,209,205,225,79,5,120,139,225,223,198,95,21,220,120,100,248,47,195,83,191,195,143,12,105,95,5,188,47,227,27,45,53,52,125,38,230,253,188,41,227,63,136,223,10,252,73,166,120,167,91,30,68,214,250,110,143,171,193,225,235,75,153,117,24,27,81,241,118,155,13,164,247,23,156,56,248,41,209,73,205,67,94,191,215,230,92,55,63,183,95,217,203,253,85,135,209,63,152,175,220,15,134,191,242,6,184,255,0,175,102,254,85,252,110,124,22,248,139,99,225,52,180,131,227,175,252,20,27,246,208,253,143,47,33,11,29,221,207,237,7,240,223,246,18,248,117,240,234,29,70,114,46,116,191,15,104,255,0,180,46,179,251,30,234,191,12,188,105,226,235,205,16,157,74,223,70,208,60,107,172,106,203,101,105,126,110,108,173,238,116,109,114,219,76,254,142,126,31,254,201,31,31,167,210,39,120,191,224,168,191,183,85,146,139,114,76,118,223,15,191,224,153,172,132,99,161,251,111,252,19,178,86,43,244,96,120,237,95,13,141,194,194,45,57,215,80,82,122,123,147,105,250,59,91,241,58,105,203,153,36,150,171,208,233,127,104,207,245,19,127,219,215,254,131,29,127,61,159,181,183,252,130,124,67,255,0,94,26,135,254,136,122,251,239,246,201,248,125,170,124,4,240,221,183,138,190,56,255,0,193,93,191,104,175,135,126,24,212,245,147,225,205,55,196,63,24,7,252,19,31,225,238,131,169,120,134,238,194,247,83,180,208,108,181,125,79,246,28,210,173,238,245,169,180,237,35,86,184,75,88,229,105,222,29,42,226,84,140,199,4,174,191,207,55,237,17,117,241,183,197,218,126,180,62,24,124,86,253,185,252,115,167,181,165,228,50,248,191,227,159,194,255,0,217,127,246,107,240,77,134,161,111,19,93,234,90,78,187,162,120,239,246,62,210,62,32,59,127,100,121,13,107,127,163,120,15,85,209,174,175,53,59,123,15,237,107,118,131,89,159,71,234,192,225,162,220,102,170,167,30,238,50,75,239,105,19,55,170,186,212,254,104,255,0,224,161,95,242,81,244,31,250,229,175,127,232,237,46,138,224,255,0,108,175,15,120,219,195,158,46,209,45,60,121,241,14,95,136,250,203,157,113,151,89,111,11,104,126,16,130,59,117,125,37,68,81,233,90,38,245,50,180,166,86,119,121,88,16,17,82,56,246,187,74,87,219,208,73,82,130,82,82,73,110,189,78,103,187,63,220,190,138,40,173,132,21,254,88,127,240,77,61,99,72,240,239,194,127,8,120,131,196,26,174,155,161,104,58,31,195,29,39,88,214,245,189,98,250,219,76,210,52,125,35,76,208,244,235,237,79,85,213,53,43,217,18,29,63,77,183,179,130,105,167,158,87,72,162,142,6,146,71,84,86,53,254,167,149,254,71,159,240,79,191,217,154,211,226,30,149,240,219,198,26,135,197,255,0,139,90,117,245,135,135,60,25,226,31,15,233,43,31,194,79,22,248,79,194,58,246,143,97,162,203,162,107,254,16,240,127,196,255,0,132,218,254,155,225,223,19,88,152,84,90,107,22,182,201,171,194,38,184,219,127,186,238,240,207,195,152,40,74,141,170,79,146,55,222,215,254,189,74,141,238,237,191,252,20,127,86,31,1,63,107,223,9,106,209,217,67,240,39,225,159,198,79,218,162,229,2,220,11,239,129,94,21,240,252,31,14,47,116,120,72,183,212,181,143,13,254,208,191,25,252,93,224,255,0,134,94,56,107,45,102,91,109,54,247,75,208,188,105,169,235,240,106,15,117,9,210,10,232,250,244,154,87,223,95,179,23,252,19,239,227,174,129,170,106,190,48,240,199,199,13,55,246,0,240,110,163,225,155,205,47,74,253,156,63,96,219,255,0,21,248,243,192,95,15,38,55,250,83,222,91,120,126,215,246,175,183,214,126,7,166,129,174,94,233,186,135,136,245,107,143,8,254,203,63,13,252,99,30,189,226,89,33,255,0,132,211,80,134,79,22,223,120,251,224,95,128,159,179,167,198,11,232,236,190,205,251,123,254,214,58,62,224,132,13,55,193,223,176,188,155,57,81,242,255,0,107,254,197,215,68,224,250,231,223,53,251,17,240,255,0,246,71,248,255,0,62,147,51,69,255,0,5,68,253,186,172,148,91,177,49,218,252,63,255,0,130,102,186,16,23,238,150,188,255,0,130,118,74,219,126,141,158,122,215,198,215,173,74,146,112,163,94,52,185,180,110,81,156,238,189,28,121,87,254,3,232,237,191,76,83,113,87,95,117,151,245,253,124,191,50,53,191,128,31,26,63,99,93,99,83,241,94,191,240,63,195,191,182,151,139,181,237,23,81,240,186,254,208,255,0,11,252,117,241,4,126,212,250,15,135,53,27,221,51,84,95,131,250,141,167,252,20,99,246,182,241,206,162,127,103,203,61,75,195,50,235,77,38,153,241,209,45,207,136,252,95,10,216,252,39,130,81,174,120,190,243,243,87,227,231,237,39,240,99,226,20,218,159,132,244,159,22,221,120,119,199,122,182,159,173,75,163,252,53,248,167,225,47,26,124,19,248,171,174,88,89,233,119,87,243,235,154,23,194,175,140,94,29,208,188,69,173,248,96,91,88,234,155,117,91,77,46,109,53,228,208,181,24,18,233,166,211,175,163,183,253,181,248,247,251,45,124,114,177,134,83,117,255,0,5,34,253,180,117,112,62,211,198,165,224,95,248,39,116,64,224,33,59,134,147,251,4,218,240,115,207,211,140,87,224,151,237,135,251,52,120,167,196,30,19,241,71,135,188,117,251,83,124,115,248,145,225,219,187,71,150,255,0,195,222,57,240,15,236,107,170,232,122,139,233,183,16,106,150,38,255,0,76,181,253,147,237,226,184,48,234,54,118,147,196,118,134,138,107,88,229,137,146,72,209,135,78,26,165,10,243,132,170,214,82,169,31,181,21,52,191,240,22,154,251,172,76,180,106,202,223,215,245,253,104,127,38,31,240,80,162,127,225,99,232,39,214,45,123,146,58,254,251,74,245,239,140,81,92,31,237,149,224,87,248,127,226,253,19,70,62,53,241,231,141,209,142,187,58,234,31,16,117,216,60,71,172,68,225,180,152,157,19,83,93,58,9,12,44,18,50,99,114,200,172,133,163,84,103,144,185,95,99,135,183,177,167,103,117,109,237,110,167,59,221,223,115,253,203,232,162,138,216,65,95,228,119,255,0,4,251,253,166,45,62,30,105,95,13,188,31,127,240,131,226,214,163,125,127,225,223,6,120,119,195,250,176,147,225,39,132,124,39,226,221,123,88,176,209,34,209,52,15,8,248,195,226,127,197,157,3,77,241,23,137,239,154,101,54,154,61,181,203,234,211,8,174,2,216,230,210,236,91,255,0,174,37,127,150,31,252,19,79,71,210,60,67,240,163,194,30,31,241,6,151,166,235,186,14,185,240,203,73,209,181,189,19,88,177,181,212,244,141,99,72,212,244,61,58,203,82,210,245,77,54,250,55,135,81,211,174,44,231,154,25,160,153,30,41,162,153,163,145,89,24,131,193,152,74,17,160,156,227,207,27,237,242,46,23,187,182,246,63,162,159,128,159,180,103,198,11,24,172,69,175,236,19,251,89,107,33,118,96,233,190,48,253,133,226,15,200,251,191,218,255,0,182,141,161,31,240,32,61,235,246,43,225,255,0,237,113,241,254,13,38,117,139,254,9,119,251,117,94,169,183,108,203,107,241,3,254,9,156,168,62,95,188,69,231,252,20,78,22,219,146,122,41,60,116,175,199,95,128,159,178,31,132,116,152,172,102,248,19,241,47,227,47,236,173,116,193,45,133,151,192,175,20,248,126,127,134,246,58,68,196,92,106,122,55,135,63,103,175,140,254,16,241,135,195,47,4,53,238,180,150,186,149,238,169,161,120,47,76,215,231,212,22,234,118,213,194,235,26,244,122,175,223,95,179,23,252,20,19,227,174,189,170,106,222,15,240,207,192,253,51,246,255,0,240,110,157,225,155,189,79,73,253,163,255,0,96,219,15,21,120,11,192,95,16,231,26,134,149,29,221,215,135,174,255,0,106,233,244,95,130,15,160,104,151,186,158,161,225,205,90,15,8,254,212,223,18,60,99,38,187,225,169,38,255,0,132,47,79,130,63,22,216,248,11,227,171,209,165,85,57,209,161,26,156,155,169,57,198,203,213,203,149,252,165,126,182,237,209,22,210,87,118,251,159,111,35,11,227,223,237,73,241,198,250,9,5,207,252,19,119,246,209,209,242,110,191,228,35,227,159,248,39,116,163,144,128,255,0,200,35,246,246,186,35,24,29,143,94,1,175,193,31,219,15,246,151,241,79,135,252,37,226,127,16,120,235,246,89,248,231,240,223,195,182,150,141,22,161,226,31,29,120,251,246,53,210,116,77,57,181,41,224,211,44,69,254,167,105,251,88,92,69,110,102,212,111,45,32,136,110,45,44,215,81,197,16,121,29,20,254,177,235,95,31,190,52,126,217,90,198,165,225,77,127,227,119,135,63,98,223,22,232,58,46,161,226,113,251,59,252,47,240,47,196,22,253,169,245,239,14,105,183,154,94,148,191,24,117,27,175,248,40,199,236,147,224,109,64,126,207,151,154,151,137,101,209,26,61,47,224,91,219,143,17,248,66,22,177,248,177,52,167,92,240,125,159,230,167,199,207,217,179,224,199,195,217,117,47,22,105,62,19,186,241,31,142,180,173,63,89,139,70,248,151,241,79,197,190,53,248,215,241,91,67,176,188,210,238,172,46,52,45,11,226,175,198,63,17,235,190,35,208,252,48,109,239,181,82,186,93,166,167,14,154,146,107,186,140,201,106,179,106,55,210,92,116,97,105,208,163,56,198,173,21,25,233,164,92,218,182,150,247,155,183,221,114,100,219,105,220,254,47,63,108,175,29,63,196,15,23,232,186,201,240,87,143,124,16,136,117,200,23,78,248,133,161,67,225,205,98,87,221,164,200,237,30,152,186,140,242,44,42,175,24,50,56,84,102,98,177,179,178,74,16,174,243,254,10,23,255,0,37,31,65,255,0,174,90,247,254,143,210,232,175,178,195,219,216,211,178,178,183,234,115,189,217,254,222,116,81,69,108,32,175,242,47,253,134,188,101,251,74,217,248,103,194,30,20,240,39,193,95,24,105,250,29,183,132,60,61,99,117,241,3,195,218,143,193,159,17,248,230,91,68,210,180,171,45,82,15,12,248,27,226,95,197,47,13,105,222,19,241,0,150,91,137,180,237,91,88,155,196,22,182,147,105,144,46,165,225,13,70,43,185,237,237,63,215,66,191,207,95,246,216,253,155,245,207,248,39,79,252,20,27,226,183,134,245,31,15,95,248,119,224,167,198,95,29,248,195,226,223,236,231,169,104,158,25,211,124,13,240,207,84,240,23,140,53,88,188,75,173,124,51,240,22,155,160,235,23,118,26,72,240,22,179,226,116,240,204,154,81,251,5,204,118,122,46,141,172,54,145,164,233,30,32,209,60,222,60,117,213,27,242,41,164,250,223,79,61,63,93,10,135,196,143,99,248,43,240,230,195,197,113,217,207,241,211,254,9,243,251,104,126,216,87,146,132,150,234,215,246,130,248,145,251,9,124,70,248,117,54,163,9,22,218,95,136,52,127,217,235,88,253,176,180,159,134,94,11,241,109,150,136,63,179,109,245,173,3,193,90,70,174,214,119,154,128,186,189,184,185,214,117,203,157,75,250,59,248,123,251,91,252,127,135,73,158,56,127,224,151,95,183,93,232,107,114,4,150,159,16,63,224,153,202,188,128,15,23,159,240,81,56,89,134,79,101,39,219,154,252,132,253,158,62,60,232,177,89,233,87,81,95,147,12,145,198,197,103,185,184,243,128,17,137,12,158,94,224,72,216,6,229,218,25,72,25,7,60,126,181,248,7,246,163,211,108,180,232,151,237,177,32,138,31,178,204,209,56,154,70,153,86,22,96,230,223,229,111,221,200,21,152,190,73,92,136,212,53,124,70,50,110,163,180,240,252,252,189,57,230,146,249,39,100,116,66,201,124,86,251,188,143,136,191,108,159,28,107,223,31,124,55,111,225,111,142,31,240,72,31,218,75,226,55,133,244,189,111,254,18,61,59,195,255,0,24,19,254,9,151,241,19,64,211,124,69,111,167,223,105,182,154,245,158,147,169,254,220,26,173,189,158,181,22,155,170,106,240,71,117,28,75,58,65,169,220,196,146,8,230,149,79,243,213,251,67,104,223,30,188,43,99,171,175,194,223,131,159,183,143,130,172,77,173,212,141,224,223,142,31,17,255,0,102,95,218,95,193,90,142,165,119,19,217,234,58,174,181,172,248,235,246,191,214,62,32,9,14,148,32,91,91,13,23,199,154,94,143,107,123,165,91,223,127,100,220,188,250,196,26,191,245,19,241,143,246,143,211,181,27,121,226,55,48,153,209,26,55,142,9,138,187,207,44,73,52,127,34,236,145,191,116,72,5,213,64,40,114,93,157,72,252,44,253,173,190,62,232,86,122,110,164,38,212,167,73,103,130,228,11,136,47,164,142,24,16,161,6,117,144,177,229,67,15,44,6,204,140,70,54,174,246,78,172,21,105,69,198,49,195,217,118,114,147,95,115,109,106,76,210,110,252,219,117,63,140,63,218,242,63,139,62,50,241,110,145,123,226,223,131,126,32,240,62,177,3,107,74,250,61,174,169,101,227,139,127,34,86,210,218,57,70,173,225,251,112,169,47,152,147,6,71,137,6,213,70,142,73,55,58,196,87,250,43,127,193,6,255,0,224,153,127,15,188,119,240,103,226,215,237,71,251,96,254,206,255,0,14,190,40,233,31,180,38,185,224,135,253,158,252,39,251,69,252,18,240,15,143,47,116,143,133,126,12,211,60,65,122,62,47,120,58,227,198,231,84,185,209,116,15,27,106,126,57,104,160,183,125,55,73,123,237,55,225,142,149,175,193,54,173,163,235,90,53,212,101,125,157,21,47,101,11,197,71,77,181,211,239,254,191,19,7,187,234,127,87,148,81,69,108,32,175,153,255,0,107,143,217,31,224,143,237,183,240,71,196,159,1,126,61,120,110,77,107,194,154,212,144,234,186,30,185,165,77,111,167,120,215,225,207,141,116,235,123,200,60,61,241,31,225,207,136,103,179,159,254,17,207,26,233,191,111,188,88,102,104,110,45,47,45,47,239,116,141,94,203,82,208,245,45,79,76,189,40,164,210,106,205,93,48,63,150,143,143,223,240,67,159,219,175,246,113,241,92,90,175,236,127,226,141,59,246,174,248,85,127,172,27,109,51,194,158,32,241,39,132,190,22,124,107,240,77,133,229,255,0,141,181,75,104,60,69,55,140,117,125,63,194,158,56,240,254,159,165,216,248,50,206,227,90,179,212,116,237,79,80,212,188,65,39,217,60,25,166,105,214,178,222,39,231,151,199,175,139,255,0,181,31,236,63,227,13,55,225,71,237,89,240,239,81,248,101,241,15,196,62,27,179,248,135,163,104,50,120,159,192,127,16,26,239,193,122,182,169,172,120,111,79,213,255,0,182,126,30,248,163,82,178,181,18,107,158,18,241,28,63,101,150,116,185,79,236,255,0,54,72,150,41,160,119,40,175,63,17,128,195,73,57,251,59,54,87,60,143,90,248,119,240,35,254,10,125,251,94,124,39,210,254,57,126,207,95,179,150,165,227,255,0,132,190,63,183,214,219,193,62,40,31,23,190,3,120,61,117,91,159,10,107,58,167,131,181,128,190,29,241,175,196,189,63,82,210,252,159,22,120,115,89,181,38,226,11,115,40,182,51,192,230,9,98,153,191,96,127,100,31,248,55,159,195,218,55,137,60,47,241,91,246,253,248,155,166,124,121,241,71,134,252,67,15,136,52,239,217,255,0,192,54,247,45,251,60,121,186,78,173,226,239,236,219,47,136,186,191,140,180,72,117,143,141,58,44,246,183,94,9,212,228,211,100,211,252,51,96,183,186,53,222,137,174,65,226,237,6,230,100,186,40,173,104,96,176,244,148,101,26,126,245,150,250,131,147,106,215,63,166,26,40,162,187,9,63,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 9334; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

