#ifndef GETVESUITE_Dryer_Dryer_SPRAY_H
#define GETVESUITE_Dryer_Dryer_SPRAY_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Dryer_Dryer_SPRAY( void )
{
    unsigned char osgData[ 5457 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,122,0,67,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,252,88,253,142,191,228,29,167,127,216,113,207,227,230,94,243,245,175,232,107,224,71,252,125,105,163,210,123,172,122,140,220,66,79,62,153,175,231,151,246,58,255,0,144,118,157,255,0,97,182,255,0,209,151,181,253,13,124,8,255,0,143,189,55,254,187,221,127,233,68,85,242,88,249,205,78,73,201,187,124,187,118,54,138,92,168,253,122,248,60,7,216,236,56,31,234,39,255,0,209,81,15,229,94,207,251,87,255,0,201,174,120,179,254,198,207,128,159,250,208,31,12,171,198,62,15,127,199,157,135,253,112,159,255,0,69,69,95,160,26,97,248,110,218,13,157,175,197,139,143,4,65,224,205,87,89,240,62,130,203,241,13,180,17,225,187,223,21,248,131,198,154,22,135,240,243,73,88,124,72,226,218,255,0,196,151,191,16,181,15,11,218,104,86,171,186,238,243,92,189,211,173,52,232,228,212,38,182,141,188,26,110,189,74,180,161,9,73,243,59,59,54,236,174,174,237,189,150,236,221,91,146,239,79,187,203,250,191,145,243,231,237,5,207,237,9,251,7,231,254,124,188,98,57,255,0,178,25,241,47,142,189,49,92,199,197,96,51,119,192,255,0,91,119,250,218,243,94,173,224,47,130,247,191,0,188,35,224,207,15,254,208,223,180,54,129,251,64,252,78,214,62,32,91,217,252,37,241,159,196,15,9,248,99,192,254,51,131,198,26,135,192,109,27,85,248,179,225,159,134,118,215,222,35,212,111,39,135,93,241,183,131,255,0,105,79,31,217,232,26,76,235,99,224,159,10,120,222,227,192,250,5,141,151,129,124,25,164,195,7,149,124,87,198,110,142,121,50,221,103,160,31,241,235,208,15,94,185,174,140,93,25,97,107,198,156,49,14,189,38,189,218,138,234,50,219,107,246,234,68,29,225,39,203,111,158,191,215,83,242,67,227,159,221,252,111,191,246,238,191,10,127,106,159,248,245,212,255,0,236,8,255,0,206,218,191,117,190,57,244,31,91,223,229,119,95,133,63,181,79,252,122,234,127,246,4,127,231,109,94,150,14,83,180,91,149,223,171,242,253,76,231,240,179,251,75,255,0,131,118,63,229,14,63,177,239,253,220,15,254,181,47,198,234,40,255,0,131,118,63,229,14,95,177,239,253,220,15,254,181,47,198,234,43,235,225,240,199,209,24,159,192,119,236,117,255,0,32,237,59,254,195,109,255,0,163,47,107,250,26,248,17,255,0,31,122,111,253,119,186,255,0,210,136,171,249,229,253,142,191,228,29,167,127,216,109,191,244,101,237,127,67,95,2,63,227,239,77,255,0,174,247,95,250,81,21,124,158,96,189,250,143,249,77,227,178,244,63,94,190,15,127,199,157,135,35,253,68,255,0,250,42,47,241,175,173,190,10,106,95,8,63,104,143,16,174,182,124,43,175,235,26,199,236,161,251,65,248,130,199,192,62,57,188,158,227,79,240,133,215,196,157,43,224,118,191,240,223,199,190,35,248,119,169,120,119,196,175,109,241,7,72,211,180,239,143,95,21,124,1,173,199,169,219,132,209,188,101,224,79,19,105,82,89,46,175,161,199,117,109,240,118,158,255,0,21,225,248,81,226,235,191,129,222,23,210,60,87,241,86,215,194,154,154,248,34,207,197,23,118,214,62,7,210,252,75,169,181,142,135,160,248,159,226,61,221,198,189,166,75,31,195,109,47,89,213,44,47,245,232,180,219,167,215,175,52,187,11,171,31,13,88,106,254,34,185,210,180,139,255,0,215,191,2,106,19,106,218,140,186,140,241,195,11,221,66,101,91,107,101,104,237,44,161,253,226,91,216,216,194,238,198,222,194,8,4,80,193,16,98,34,134,20,141,126,85,21,230,199,253,143,5,83,22,159,239,235,243,83,167,105,123,208,181,185,165,36,187,236,186,26,167,119,24,187,36,172,221,251,14,248,221,240,255,0,192,222,59,62,9,184,241,183,135,134,184,223,15,254,33,120,103,226,71,132,174,173,117,93,95,195,186,231,135,252,87,225,194,240,71,168,104,62,38,240,237,229,182,161,163,27,221,3,83,241,6,137,171,69,4,235,109,173,120,115,197,154,207,134,245,136,47,244,13,107,86,211,47,127,51,174,62,40,248,103,226,190,137,172,234,250,21,166,191,160,234,158,30,241,14,173,225,79,28,120,11,198,150,58,126,145,241,7,225,175,141,116,237,46,194,255,0,80,240,87,143,180,93,39,87,212,109,44,117,232,244,205,91,70,190,182,185,211,181,29,79,67,214,244,125,123,76,241,31,134,117,157,119,195,26,206,141,173,234,63,171,223,17,135,250,40,255,0,174,209,142,123,252,144,245,175,204,127,141,218,143,197,237,79,198,95,10,188,33,224,175,10,104,94,53,240,134,181,226,47,27,232,126,51,251,69,196,58,95,140,252,17,102,124,7,226,47,21,248,95,196,158,23,213,111,124,71,12,94,35,209,103,241,135,135,161,240,244,222,19,131,72,212,53,93,67,85,248,167,167,107,58,69,254,151,30,147,174,105,222,38,156,4,190,185,66,88,58,173,58,148,211,157,57,202,86,81,75,89,70,218,252,93,53,189,199,85,114,167,43,221,109,162,243,90,159,150,31,28,250,15,173,239,242,187,175,194,159,218,167,254,61,117,63,251,2,63,243,182,175,221,111,142,120,199,28,140,223,127,237,221,126,20,254,213,63,241,235,169,255,0,216,17,255,0,157,181,119,96,126,8,244,183,252,3,9,252,44,254,210,255,0,224,221,143,249,67,151,236,123,255,0,119,3,255,0,173,75,241,186,138,63,224,221,143,249,67,151,236,123,255,0,119,3,255,0,173,75,241,186,138,251,40,124,17,244,95,145,137,252,7,126,199,95,242,14,211,191,236,54,223,250,50,246,191,161,175,129,31,241,247,166,255,0,215,123,175,253,40,138,191,158,95,216,235,254,65,218,119,253,134,219,255,0,70,94,215,244,53,240,35,254,62,244,223,250,239,117,255,0,165,17,87,201,102,42,245,38,175,163,255,0,128,111,29,151,161,251,3,240,98,234,230,223,79,138,40,46,39,134,43,219,25,173,175,34,138,105,35,142,238,217,100,178,189,91,123,148,70,2,120,5,229,165,164,161,28,21,18,90,199,38,55,162,145,250,85,240,199,239,67,255,0,94,99,249,73,95,153,223,7,191,227,206,195,254,184,79,255,0,162,162,175,211,31,134,63,122,15,250,243,31,202,74,249,170,242,118,190,247,251,186,104,111,4,172,157,246,233,247,110,119,223,17,191,227,212,127,215,104,255,0,244,8,107,243,191,226,85,229,222,157,123,253,161,167,221,220,216,223,216,223,73,123,99,125,103,60,182,183,150,87,118,176,164,214,215,86,151,80,58,189,189,204,115,70,143,27,163,43,35,160,101,32,128,107,244,67,226,55,252,122,143,250,237,31,254,129,13,126,115,124,86,235,119,255,0,93,110,255,0,244,150,166,142,142,60,186,61,63,64,150,215,189,172,126,72,252,115,233,248,222,255,0,43,186,252,41,253,170,127,227,215,83,255,0,176,35,255,0,59,106,253,214,248,231,208,125,111,127,149,221,126,20,254,213,63,241,235,169,255,0,216,17,255,0,157,181,125,30,10,252,170,238,239,254,24,231,159,194,207,237,47,254,13,216,255,0,148,57,126,199,191,247,112,63,250,212,191,27,168,163,254,13,216,255,0,148,57,126,199,191,247,112,63,250,212,191,27,168,175,177,135,193,31,69,249,24,159,192,119,236,117,255,0,32,237,59,254,195,109,255,0,163,47,107,250,26,248,17,255,0,31,122,111,253,119,186,255,0,210,136,171,249,229,253,142,179,253,159,167,114,63,228,52,220,119,63,188,188,207,233,95,208,215,192,143,248,251,211,127,235,189,207,254,148,69,95,39,152,127,18,95,215,99,120,236,143,215,175,131,223,241,231,97,255,0,92,39,255,0,209,81,87,233,143,195,31,189,7,253,121,143,229,37,126,103,124,30,255,0,143,59,15,250,225,63,254,138,138,191,76,62,24,231,48,224,19,254,134,61,187,73,158,126,134,190,98,187,73,53,187,95,46,198,241,94,234,59,255,0,136,223,241,234,63,235,180,127,250,4,53,249,205,241,91,173,223,253,117,187,255,0,210,90,253,25,248,141,143,178,142,191,235,147,183,96,176,131,223,174,107,243,155,226,183,45,118,57,226,91,172,156,113,131,104,78,79,28,242,49,249,209,71,120,250,47,208,39,240,179,242,71,227,159,65,245,189,254,87,117,248,83,251,84,255,0,199,174,167,255,0,96,71,254,118,213,251,173,241,207,238,131,158,51,122,125,6,54,222,30,73,252,125,58,87,225,79,237,83,159,179,106,125,63,228,8,254,253,237,125,254,191,253,110,149,244,88,47,133,127,93,140,36,174,154,63,180,191,248,55,99,254,80,229,251,30,255,0,221,192,255,0,235,82,252,110,162,143,248,55,99,254,80,229,251,30,255,0,221,192,250,255,0,209,210,252,110,245,20,87,216,195,224,135,162,252,140,15,226,111,246,150,253,155,124,79,255,0,4,217,253,184,126,45,254,202,186,192,213,100,240,95,135,124,78,60,87,240,95,197,58,244,83,52,254,56,248,49,227,41,167,213,190,28,120,152,248,139,80,240,198,139,109,226,77,102,218,198,107,157,7,196,215,154,86,156,52,120,60,83,225,173,99,77,211,204,191,98,103,175,209,159,129,159,18,47,29,180,249,224,187,0,72,130,104,11,69,2,128,100,127,180,21,253,245,136,7,120,138,104,249,29,109,247,0,76,128,87,244,81,255,0,5,239,255,0,130,119,107,223,183,39,236,163,167,248,247,225,68,114,73,251,66,254,201,210,120,207,226,151,195,189,38,214,195,198,90,230,173,241,47,193,23,158,28,142,231,226,159,193,47,12,232,94,20,154,227,206,241,175,136,71,133,60,23,121,160,204,218,54,173,119,46,183,224,125,63,67,182,109,42,219,93,212,53,59,111,227,155,246,80,248,158,154,197,150,155,182,117,121,68,112,72,161,36,49,77,186,70,183,103,216,165,218,85,243,65,129,247,74,230,66,140,228,69,195,1,224,230,120,91,73,212,138,186,151,252,3,72,61,26,234,127,77,95,8,124,127,171,152,237,182,94,159,36,38,98,242,96,176,153,12,120,138,73,148,72,214,227,39,202,117,65,203,114,153,200,230,191,77,190,25,120,211,95,124,52,90,158,66,197,18,41,251,29,136,194,60,172,84,0,214,124,159,38,69,30,184,110,78,120,175,196,143,129,62,35,138,234,223,78,144,74,66,110,130,50,241,229,119,66,224,203,28,91,64,83,48,17,220,144,252,6,31,102,57,66,126,99,250,229,240,146,228,220,62,158,228,133,145,225,86,145,65,249,113,182,4,228,159,225,47,17,108,100,176,218,51,145,205,124,150,38,10,55,77,107,255,0,12,116,195,102,125,49,241,7,196,218,227,90,57,107,238,86,7,156,127,162,217,252,178,198,200,170,252,91,30,118,42,224,119,198,14,107,243,167,226,215,139,181,21,23,27,47,28,179,43,16,203,107,107,129,45,211,180,80,144,26,17,192,129,70,71,96,195,141,217,199,221,95,19,228,217,96,146,59,149,141,85,12,152,220,217,81,49,114,89,123,128,87,63,81,145,95,151,31,27,117,117,137,174,162,118,216,161,94,50,138,238,22,72,146,5,66,207,199,239,92,51,202,225,66,159,150,34,56,198,227,56,120,221,165,107,171,255,0,144,77,232,151,115,243,231,227,143,140,229,84,159,204,153,63,118,178,40,40,98,145,162,121,196,211,204,89,82,216,228,199,6,240,67,15,155,112,232,196,10,252,43,253,171,254,34,88,219,65,125,3,148,105,90,222,91,105,87,122,25,22,16,203,36,145,162,59,196,124,192,22,24,162,112,29,101,145,192,60,145,95,165,63,180,111,142,210,206,13,78,229,164,11,176,73,46,100,153,98,129,100,103,133,227,82,191,50,71,60,97,237,193,39,204,1,22,77,228,96,176,241,223,248,37,239,236,159,251,66,126,212,223,180,253,151,237,75,240,219,193,218,54,191,224,47,217,39,226,47,133,60,75,164,63,143,116,63,136,115,248,35,197,223,25,244,225,39,138,252,15,167,89,93,120,43,196,250,16,213,46,252,51,168,219,120,111,197,26,149,181,182,191,229,164,183,190,28,182,213,244,139,253,43,88,154,54,250,44,42,167,74,10,165,103,201,78,22,191,225,247,152,202,237,89,110,207,234,111,254,9,73,251,1,126,211,191,6,191,224,158,191,179,23,129,252,103,251,101,254,212,31,179,247,137,27,193,122,199,141,245,31,130,127,15,60,15,251,38,31,15,124,48,31,21,60,109,226,159,138,86,94,15,188,183,253,161,63,98,173,111,198,26,95,140,236,244,255,0,25,90,71,226,75,13,99,85,190,109,63,196,99,85,177,177,156,233,182,246,106,165,126,193,252,6,214,62,43,235,255,0,10,60,41,171,252,112,176,240,190,153,241,70,239,251,115,254,18,123,31,5,233,58,190,135,225,168,124,143,18,107,22,218,47,246,110,151,174,248,135,85,187,181,221,225,216,116,151,155,205,191,159,125,196,146,200,158,84,110,144,198,87,213,83,113,156,33,56,59,194,73,53,232,210,107,240,48,61,126,191,128,95,248,44,39,236,47,63,252,19,223,246,212,143,226,167,194,31,135,43,225,15,216,243,246,142,155,77,214,188,20,60,43,225,239,236,175,134,191,10,254,53,205,14,162,191,16,62,11,194,208,120,158,236,104,137,168,174,151,121,226,205,30,7,176,209,244,212,211,60,89,170,104,94,23,176,58,103,131,47,126,199,253,253,87,197,255,0,183,255,0,236,81,224,15,248,40,23,236,187,227,239,217,183,199,122,171,120,78,227,196,13,165,248,135,192,31,18,108,188,57,160,248,155,94,248,91,241,31,195,55,139,127,225,159,26,104,86,26,244,24,100,42,111,244,173,98,11,75,157,54,247,84,240,215,138,53,189,18,13,91,77,26,163,222,69,53,169,42,212,220,31,203,212,105,217,167,216,254,76,255,0,102,31,27,166,163,21,138,37,196,108,101,137,21,100,51,134,96,1,150,84,145,154,12,36,69,34,107,147,141,199,203,243,16,2,6,208,255,0,187,223,3,181,53,184,183,180,43,42,55,158,179,39,1,148,0,209,73,116,85,15,103,243,103,136,96,229,179,46,222,160,237,254,79,255,0,101,95,21,248,191,225,255,0,140,252,77,240,143,226,110,158,124,61,241,63,225,31,141,188,77,240,207,226,39,134,100,213,172,53,127,236,111,26,120,11,95,191,240,215,138,116,86,213,180,75,203,139,45,84,91,120,143,76,188,183,146,254,206,226,107,41,149,79,217,38,158,216,199,35,127,78,191,179,38,181,21,245,189,144,105,131,28,195,18,72,34,112,100,153,201,200,42,177,0,160,47,148,7,28,228,156,244,11,240,89,149,7,78,82,233,111,248,7,77,55,119,182,232,251,83,226,220,194,27,16,24,129,231,65,28,93,1,108,31,182,109,40,7,36,249,134,48,78,8,195,28,142,227,241,111,246,141,241,68,118,118,250,156,158,116,110,132,74,174,119,72,133,124,223,62,226,92,170,14,25,224,146,117,192,28,148,11,130,192,161,253,131,248,235,127,4,90,78,241,32,46,214,12,97,82,146,48,103,253,228,145,163,54,213,224,137,35,244,226,65,208,147,143,231,35,246,198,248,143,109,163,218,234,147,73,116,35,133,35,184,148,16,207,108,90,39,46,22,6,148,198,162,23,89,99,136,36,140,6,225,35,169,110,88,86,88,40,115,74,41,45,91,255,0,33,205,234,151,99,242,187,227,84,95,21,126,62,124,80,240,183,192,63,129,62,18,214,62,36,252,97,248,163,174,191,135,188,11,224,191,13,53,189,197,246,173,171,77,29,196,247,55,23,55,207,168,90,193,161,232,122,126,143,29,253,254,161,169,222,79,109,97,165,105,86,186,142,161,168,220,218,233,214,23,23,48,255,0,87,159,177,159,193,191,219,167,246,32,253,159,188,23,240,7,224,247,236,66,186,86,151,161,219,38,173,227,79,17,105,222,61,253,156,52,221,83,226,111,196,205,74,202,194,31,25,252,78,241,75,92,252,117,188,184,184,215,181,91,219,24,153,99,154,242,230,61,51,79,179,176,209,52,246,135,72,210,180,235,72,62,91,255,0,131,117,127,97,171,111,19,92,248,155,254,10,125,241,60,73,127,172,120,174,239,226,15,194,111,217,143,195,119,250,103,132,53,45,31,69,240,118,145,172,195,225,159,136,159,27,116,237,65,228,188,213,116,15,27,222,248,171,72,241,199,131,108,160,219,162,92,233,218,62,153,226,82,227,88,210,124,95,100,246,95,214,53,125,140,114,202,21,168,66,53,211,125,108,157,151,221,99,15,104,211,110,61,79,32,248,11,171,124,81,215,62,19,248,83,84,248,209,224,249,188,5,241,46,235,251,119,254,18,95,9,207,170,120,111,90,151,73,242,124,75,172,91,232,251,181,63,8,235,58,134,157,115,231,232,17,105,87,35,236,247,147,108,23,130,57,124,185,210,88,144,175,95,162,189,58,112,141,56,66,156,116,141,52,162,189,18,178,51,10,40,162,172,15,229,91,254,11,247,251,28,234,158,11,241,207,129,191,224,164,191,12,244,205,10,195,195,154,53,135,134,254,25,126,214,226,59,239,12,120,122,241,227,159,196,218,15,133,190,7,124,88,77,63,251,38,202,227,198,122,209,212,60,70,124,29,173,221,220,235,23,250,140,22,41,224,120,180,221,46,45,39,72,214,111,236,244,127,97,63,29,193,175,217,232,47,36,210,73,230,193,106,35,219,25,17,151,140,163,56,93,185,4,240,129,62,98,129,100,202,48,25,65,253,50,124,72,248,123,225,15,139,159,14,252,123,240,167,226,14,146,218,247,128,190,39,120,47,197,63,15,124,111,161,166,165,171,104,207,173,120,67,198,154,29,247,134,252,75,164,166,175,160,223,90,223,105,77,115,163,106,87,176,139,155,43,155,123,184,12,222,109,180,240,204,169,34,255,0,36,31,179,255,0,195,63,18,254,197,159,182,71,197,47,217,3,197,183,90,229,214,155,224,63,17,218,234,31,9,252,85,175,54,161,53,239,196,15,130,222,42,243,111,254,25,248,185,245,171,159,10,232,214,254,43,215,147,195,229,52,111,17,94,233,90,114,105,48,248,191,193,254,34,210,244,233,30,45,52,200,191,59,158,97,57,161,245,136,45,87,197,250,26,210,110,233,127,93,15,215,207,218,147,88,135,74,240,169,119,119,89,30,192,44,101,0,36,153,32,132,46,222,204,55,33,12,27,10,68,156,146,50,181,252,214,233,223,0,124,107,255,0,5,23,253,179,188,25,251,42,120,51,80,75,15,15,201,60,127,16,126,60,120,142,31,16,232,254,28,214,124,25,251,60,248,123,196,222,25,209,126,35,248,135,195,141,170,88,234,45,127,227,75,136,245,221,63,69,208,109,198,139,168,91,190,181,226,157,54,227,81,182,139,65,180,212,239,180,255,0,219,191,248,41,87,196,187,47,135,191,11,174,245,155,187,196,180,72,188,56,215,36,201,112,98,68,17,89,60,198,83,33,42,21,0,115,184,147,128,34,25,82,14,83,235,127,248,35,151,236,84,223,178,239,236,232,255,0,22,60,123,164,120,131,73,253,162,255,0,106,203,63,11,124,71,248,193,166,107,215,222,38,182,127,6,120,114,192,120,134,243,224,223,194,165,240,71,136,116,45,42,79,2,107,218,7,131,60,95,112,222,37,178,184,177,151,81,143,197,222,35,241,5,164,186,173,254,147,101,161,195,97,193,144,97,125,164,157,89,124,48,254,151,252,18,234,187,55,221,159,171,158,26,240,215,135,60,23,225,207,15,248,59,193,222,31,209,60,39,225,31,9,232,154,87,134,188,45,225,111,13,105,86,26,23,135,60,53,225,205,10,194,13,47,67,240,255,0,135,244,61,46,8,173,116,109,18,207,76,181,181,182,180,180,182,138,56,45,224,182,142,24,99,72,209,84,109,209,69,125,137,206,20,81,69,0,20,81,69,0,21,249,189,255,0,5,29,253,152,53,159,141,126,14,248,113,241,147,192,4,159,138,159,178,246,187,226,95,30,105,122,76,122,111,140,181,253,71,226,7,195,93,91,66,70,248,157,240,183,195,58,23,133,47,156,73,227,109,74,111,13,248,47,82,209,103,109,35,86,187,184,212,124,15,7,135,173,127,178,237,252,69,168,106,150,223,164,52,84,85,167,26,180,231,78,106,241,154,179,26,109,52,214,232,252,94,209,191,103,203,239,218,155,246,179,248,51,227,205,70,230,8,62,13,126,204,246,255,0,14,254,46,235,81,73,97,227,40,191,225,97,252,76,188,210,181,77,127,225,55,135,124,47,226,93,26,247,79,178,129,252,63,226,43,31,14,248,175,92,221,121,122,226,218,207,65,209,245,13,10,243,75,241,100,183,118,31,180,53,231,159,13,60,11,224,143,0,104,55,122,79,128,252,29,225,95,4,233,83,107,55,190,118,153,225,31,15,105,30,27,211,229,254,192,134,211,193,58,23,153,101,163,89,195,28,159,98,240,103,133,124,47,164,90,101,79,217,180,207,13,216,88,67,178,210,206,222,40,253,14,185,176,24,106,120,92,53,58,80,215,171,125,91,127,213,135,41,57,59,176,162,138,43,176,144,162,138,40,3,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 5457; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

