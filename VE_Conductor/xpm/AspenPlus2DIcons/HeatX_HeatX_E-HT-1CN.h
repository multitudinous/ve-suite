#ifndef GETVESUITE_HeatX_HeatX_E-HT-1CN_H
#define GETVESUITE_HeatX_HeatX_E-HT-1CN_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_HeatX_HeatX_E-HT-1CN( void )
{
    unsigned char osgData[ 4777 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,53,0,147,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,250,43,246,160,255,0,130,206,126,200,159,183,167,195,239,218,155,199,127,12,116,175,141,159,179,203,124,73,255,0,130,76,126,218,255,0,178,237,175,132,126,52,252,60,181,211,252,79,251,85,252,82,248,237,165,124,63,215,127,100,40,180,235,143,128,62,37,241,198,143,117,225,239,3,127,103,126,209,150,176,222,252,64,213,188,52,190,23,185,253,169,238,143,134,163,186,181,241,31,141,238,180,207,185,162,255,0,131,175,255,0,98,41,181,91,221,18,47,217,23,254,10,24,218,174,159,167,233,154,173,229,159,252,32,63,178,168,120,52,253,102,231,87,180,211,46,90,70,253,176,2,50,203,115,161,106,202,21,88,178,155,50,93,84,50,22,252,78,253,144,191,101,211,175,254,201,191,178,254,186,52,57,101,254,219,253,157,254,10,106,254,104,181,145,132,191,218,95,13,124,53,120,36,222,34,195,6,243,247,117,231,57,163,195,95,178,233,159,246,178,248,215,160,255,0,98,72,127,179,127,103,111,217,131,87,242,133,164,153,67,174,124,73,253,175,44,247,149,17,124,187,191,225,31,192,61,252,172,118,227,196,142,101,21,95,17,100,155,91,239,246,90,143,127,63,188,213,194,201,43,255,0,78,199,247,3,251,15,254,219,223,179,223,252,20,47,246,117,240,87,237,53,251,53,248,170,227,95,240,47,139,32,134,219,87,208,117,203,88,116,127,31,252,51,241,138,105,122,94,173,172,124,54,248,161,225,104,239,39,62,23,241,205,133,150,179,164,206,241,44,247,54,58,142,157,172,105,218,246,133,127,171,120,115,87,209,245,123,250,150,127,182,215,194,171,159,137,94,25,248,65,62,129,241,7,78,248,135,175,254,215,126,56,253,139,245,45,2,243,74,240,228,131,193,191,20,124,37,251,45,248,239,246,210,208,245,255,0,18,106,186,127,138,231,178,186,240,7,137,127,102,127,9,120,115,196,186,37,214,143,113,170,234,48,127,194,214,208,52,111,17,233,94,31,241,5,183,138,52,175,13,255,0,158,239,252,19,210,255,0,246,179,253,150,252,9,240,207,226,167,236,121,241,135,197,95,6,60,81,241,91,246,111,248,75,99,227,214,210,244,79,10,120,179,195,254,46,208,96,240,231,135,117,221,9,181,239,5,124,64,240,174,183,161,234,122,174,157,171,223,235,141,164,106,146,105,199,85,210,96,241,70,181,109,167,94,89,218,235,218,204,23,255,0,125,234,223,180,175,252,20,39,81,179,240,206,183,103,226,111,5,104,95,28,116,15,218,94,231,246,180,212,255,0,105,109,43,225,221,140,223,21,60,103,241,146,227,246,97,215,191,99,21,212,252,75,225,29,114,214,243,225,213,174,137,23,236,199,171,105,158,22,142,195,71,240,38,147,9,79,9,105,250,212,158,111,136,228,213,117,141,79,166,121,141,37,202,163,36,164,164,148,147,215,75,235,107,117,237,114,84,27,87,242,63,190,170,43,248,71,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,240,223,191,240,90,207,250,61,29,115,255,0,17,247,246,86,255,0,232,117,171,254,210,194,255,0,51,251,191,224,135,36,189,15,238,226,138,254,17,255,0,225,191,127,224,181,159,244,122,58,231,254,35,239,236,173,255,0,208,235,71,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,254,210,194,255,0,51,251,191,224,135,35,238,143,238,226,138,254,17,255,0,225,191,127,224,181,159,244,122,58,231,254,35,239,236,173,255,0,208,235,71,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,254,210,194,255,0,51,251,191,224,135,35,238,143,238,226,138,254,17,255,0,225,191,127,224,181,159,244,122,58,231,254,35,239,236,173,255,0,208,235,71,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,254,210,194,255,0,51,251,191,224,135,35,238,143,238,226,138,254,17,255,0,225,191,127,224,181,159,244,122,58,231,254,35,239,236,173,255,0,208,235,71,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,254,210,194,255,0,51,251,191,224,135,35,238,143,238,226,138,254,17,255,0,225,191,127,224,181,159,244,122,58,231,254,35,239,236,173,255,0,208,235,94,43,251,64,126,212,191,240,85,143,142,255,0,12,181,191,130,255,0,28,191,107,191,26,248,151,224,231,196,226,190,21,248,149,225,45,23,225,151,192,239,135,141,227,79,8,92,65,115,123,170,248,67,88,241,63,195,127,131,218,54,180,158,20,212,99,177,91,109,99,78,181,212,224,183,215,52,185,174,244,61,98,27,253,15,81,212,244,219,177,102,56,87,246,159,221,255,0,4,57,36,126,231,252,102,255,0,131,167,127,224,157,127,10,124,123,241,111,194,94,22,248,113,251,87,126,208,190,17,248,57,172,223,104,154,231,199,15,128,222,13,248,33,173,124,25,241,53,198,143,225,205,51,196,122,230,163,240,251,95,241,255,0,237,7,225,205,75,198,94,24,182,131,82,242,224,214,109,116,131,164,107,75,106,117,47,14,223,107,26,37,214,157,170,222,243,95,241,21,87,236,101,207,252,97,231,252,20,59,143,250,145,63,101,15,203,254,79,19,175,95,202,191,150,109,35,246,112,77,55,246,74,255,0,130,154,95,38,151,43,47,128,97,248,173,27,78,208,187,152,36,176,253,135,62,11,120,166,73,30,69,86,3,247,154,155,201,146,217,253,238,227,130,113,95,172,31,240,200,199,254,128,18,140,118,251,36,191,76,113,17,174,122,185,164,105,221,168,167,105,91,240,139,253,67,147,91,95,83,237,111,21,255,0,193,229,95,240,77,63,4,107,247,254,23,241,79,236,225,255,0,5,4,210,53,237,51,236,191,111,211,229,248,99,251,54,92,61,191,219,108,173,245,11,109,211,89,254,214,18,70,251,236,238,237,223,229,115,129,38,14,24,16,10,255,0,63,95,248,42,255,0,132,7,129,63,111,223,143,126,20,242,13,175,246,87,252,42,223,244,118,141,163,104,254,221,240,91,225,206,165,202,21,24,207,219,55,123,238,205,21,233,211,159,180,167,78,127,207,20,254,244,159,234,75,86,109,31,233,73,255,0,4,222,248,18,250,191,252,19,191,246,10,213,133,128,113,170,126,197,255,0,178,230,162,27,108,103,127,219,126,7,248,26,231,119,45,158,124,218,60,11,240,37,229,255,0,130,135,254,212,122,72,176,25,177,253,140,63,96,189,68,166,216,254,95,237,79,142,63,240,82,11,96,221,123,157,32,250,253,202,250,183,254,9,121,241,55,64,176,255,0,130,103,255,0,193,59,172,103,22,198,107,47,216,91,246,72,180,151,116,108,91,205,182,248,3,240,254,23,220,66,242,119,33,252,168,248,127,241,55,195,233,255,0,5,47,253,173,239,202,219,121,55,63,176,183,252,19,182,210,60,198,219,124,219,47,143,223,240,84,41,166,218,187,126,246,219,248,51,236,5,126,121,10,149,62,183,141,209,232,231,255,0,167,98,118,53,164,124,218,63,31,127,224,153,127,179,149,191,141,127,101,31,217,7,81,146,194,27,147,125,251,24,252,1,213,30,95,33,29,139,92,252,52,248,122,193,100,101,133,193,42,146,140,2,160,141,236,73,203,115,250,75,255,0,12,117,101,255,0,64,136,191,240,21,127,249,2,188,223,254,9,23,241,51,195,126,28,253,138,127,98,104,174,236,180,225,113,31,236,51,251,55,218,200,243,20,127,49,163,248,79,240,213,25,200,121,130,172,133,224,98,71,13,215,140,115,95,171,159,240,188,252,39,255,0,62,218,55,253,241,109,255,0,201,21,142,46,181,85,94,170,87,183,51,235,230,17,140,108,180,63,58,255,0,225,142,172,191,232,17,23,254,2,175,255,0,32,81,255,0,12,117,101,255,0,64,136,191,240,21,127,249,2,191,69,63,225,121,248,79,254,125,180,111,251,226,219,255,0,146,40,255,0,133,231,225,63,249,246,209,191,239,139,111,254,72,174,117,94,178,183,101,230,87,44,123,31,157,127,240,199,86,95,244,8,139,255,0,1,87,255,0,144,40,255,0,134,58,178,255,0,160,68,95,248,10,191,252,129,95,162,159,240,188,252,39,255,0,62,218,55,253,241,109,255,0,201,20,127,194,243,240,159,252,251,104,223,247,197,183,255,0,36,81,245,138,223,211,11,46,200,252,235,255,0,134,58,178,255,0,160,68,95,248,10,191,252,129,71,252,49,213,151,253,2,34,255,0,192,85,255,0,228,10,253,20,255,0,133,231,225,63,249,246,209,191,239,139,111,254,72,163,254,23,159,132,255,0,231,219,70,255,0,190,45,191,249,34,143,172,86,254,152,89,118,71,231,95,252,49,213,151,253,2,34,255,0,192,85,255,0,228,10,63,225,142,172,191,232,17,23,254,2,175,255,0,32,87,232,167,252,47,63,9,255,0,207,182,141,255,0,124,91,127,242,69,31,240,188,252,39,255,0,62,218,55,253,241,109,255,0,201,20,125,98,183,244,194,203,178,63,58,255,0,225,142,172,191,232,17,23,254,2,175,255,0,32,81,255,0,12,117,101,255,0,64,136,191,240,21,127,249,2,191,69,63,225,121,248,79,254,125,180,111,251,226,219,255,0,146,40,255,0,133,231,225,63,249,246,209,191,239,139,111,254,72,163,235,21,191,166,22,93,145,249,215,255,0,12,117,101,255,0,64,136,143,253,186,175,255,0,32,87,146,252,100,253,146,172,244,207,13,233,87,139,164,68,89,60,71,105,183,117,178,237,102,58,110,172,72,112,182,209,239,66,129,129,4,225,129,63,46,122,126,183,127,194,243,240,159,252,251,104,223,247,197,183,255,0,36,87,144,252,105,248,201,225,61,71,194,218,124,13,103,165,56,95,16,90,203,251,165,136,30,52,221,85,50,198,25,203,99,247,157,198,63,26,168,86,172,231,31,47,63,65,56,199,170,177,252,239,107,159,7,126,207,251,12,255,0,193,124,111,163,182,71,143,194,75,241,249,22,85,17,17,18,217,127,193,38,191,101,93,108,5,218,216,194,165,232,35,3,190,122,230,191,122,255,0,225,158,28,127,204,52,1,219,229,143,167,226,213,249,89,227,31,29,104,169,251,5,255,0,193,198,118,11,13,180,50,107,39,246,140,251,36,104,55,237,251,71,252,17,183,246,63,211,83,203,113,184,96,220,65,33,235,193,99,156,87,244,49,255,0,11,103,195,103,144,182,157,6,63,116,199,143,251,231,211,53,219,141,169,81,67,75,252,107,255,0,77,83,100,195,117,233,250,159,228,185,255,0,7,10,248,120,248,83,254,11,1,251,93,232,6,33,17,211,255,0,225,65,15,47,106,252,191,106,253,152,62,10,222,231,129,223,237,25,255,0,129,81,93,143,252,28,169,171,219,107,159,240,90,223,219,71,84,179,8,45,174,191,225,156,252,189,138,2,254,227,246,77,248,17,110,248,4,127,207,72,94,138,251,124,47,251,174,26,251,251,56,127,233,40,228,150,239,212,254,188,255,0,96,223,218,91,79,208,191,97,191,216,203,67,146,238,85,125,23,246,80,253,157,180,167,80,209,0,27,78,248,67,224,235,70,28,220,3,128,97,199,32,123,138,95,7,254,210,214,16,254,220,159,180,78,185,246,185,66,106,63,178,135,236,101,165,41,221,22,89,180,111,139,255,0,183,149,219,41,205,198,56,26,250,30,9,234,122,112,79,242,197,240,67,254,10,67,224,223,3,124,23,248,67,224,155,191,17,105,16,93,120,59,225,127,128,60,45,115,4,186,253,172,18,197,63,135,188,41,164,233,19,69,36,76,185,138,69,146,205,131,41,229,74,149,235,75,164,255,0,193,72,188,25,105,241,163,199,254,54,111,17,233,11,109,226,15,133,255,0,8,188,45,12,231,94,181,88,228,159,193,254,43,248,221,171,92,68,146,237,196,146,36,126,56,181,44,163,148,19,171,31,190,43,198,142,91,53,136,196,201,211,210,163,122,250,206,44,213,212,189,172,237,107,126,135,237,191,236,129,251,90,88,252,60,253,149,127,101,27,39,212,197,168,211,127,101,255,0,129,218,1,150,107,161,8,105,44,190,26,248,70,41,109,215,19,70,1,73,44,228,227,37,138,200,187,135,25,175,165,127,225,224,26,103,253,12,54,255,0,248,48,79,254,88,215,159,255,0,193,9,63,224,140,158,39,253,175,126,10,252,26,253,164,191,110,127,15,248,167,195,191,178,237,183,194,79,8,233,255,0,2,254,8,29,107,198,95,15,60,109,241,213,159,192,186,86,133,105,241,115,197,90,183,134,53,125,51,88,240,103,193,59,91,68,150,127,11,193,111,113,105,169,248,210,245,224,241,10,75,103,224,123,45,38,111,136,191,209,119,252,56,31,254,9,61,255,0,70,203,226,47,252,73,207,218,223,255,0,159,189,116,60,149,77,202,114,105,57,54,237,234,239,253,127,86,74,165,149,174,207,193,31,248,120,6,153,255,0,67,13,191,254,12,19,255,0,150,52,127,195,192,52,207,250,24,109,255,0,240,96,159,252,177,175,222,239,248,112,63,252,18,123,254,141,151,196,95,248,147,159,181,191,255,0,63,122,63,225,192,255,0,240,73,239,250,54,95,17,127,226,78,126,214,255,0,252,253,232,254,195,135,243,71,241,255,0,47,33,251,95,55,253,124,207,193,31,248,120,6,153,255,0,67,13,191,254,12,19,255,0,150,52,127,195,192,52,207,250,24,109,255,0,240,96,159,252,177,175,222,239,248,112,63,252,18,123,254,141,151,196,95,248,147,159,181,191,255,0,63,122,63,225,192,255,0,240,73,239,250,54,95,17,127,226,78,126,214,255,0,252,253,232,254,195,135,243,71,241,255,0,47,32,246,190,111,250,249,159,130,63,240,240,13,51,254,134,27,127,252,24,39,255,0,44,104,255,0,135,128,105,159,244,48,219,255,0,224,193,63,249,99,95,189,223,240,224,127,248,36,247,253,27,47,136,191,241,39,63,107,127,254,126,244,127,195,129,255,0,224,147,223,244,108,190,34,255,0,196,156,253,173,255,0,249,251,209,253,135,15,230,143,227,254,94,65,237,124,223,245,243,63,4,127,225,224,26,103,253,12,54,255,0,248,48,79,254,88,209,255,0,15,0,211,63,232,97,183,255,0,193,130,127,242,198,191,123,191,225,192,255,0,240,73,239,250,54,95,17,127,226,78,126,214,255,0,252,253,232,255,0,135,3,255,0,193,39,191,232,217,124,69,255,0,137,57,251,91,255,0,243,247,163,251,14,31,205,31,199,252,188,131,218,249,191,235,230,126,8,255,0,195,192,52,207,250,24,109,255,0,240,96,159,252,177,163,254,30,1,166,127,208,195,111,255,0,131,4,255,0,229,141,126,247,127,195,129,255,0,224,147,223,244,108,190,34,255,0,196,156,253,173,255,0,249,251,209,255,0,14,7,255,0,130,79,127,209,179,120,139,255,0,18,115,246,183,255,0,231,239,71,246,28,63,154,63,143,249,121,3,170,250,95,239,63,4,127,225,224,58,111,253,12,54,255,0,248,48,79,233,169,86,46,183,251,115,88,120,128,105,186,108,90,196,55,133,245,40,228,104,35,189,243,36,101,142,210,240,149,65,21,219,149,44,72,92,237,63,123,31,122,190,198,253,191,63,224,155,255,0,240,79,79,217,144,124,111,111,132,31,178,46,131,227,3,240,71,254,9,139,251,115,254,218,126,51,30,35,253,161,191,109,175,16,127,194,19,241,67,224,204,223,8,211,246,79,208,188,124,190,23,253,168,180,255,0,248,70,254,31,248,234,53,253,169,12,54,183,95,100,212,124,79,255,0,10,19,90,62,27,213,109,71,133,188,69,95,86,254,214,127,240,110,199,236,147,226,255,0,131,58,220,95,177,78,147,168,254,206,31,180,135,135,238,160,241,55,195,191,22,248,159,226,231,199,47,137,31,14,188,89,127,166,90,95,69,39,195,63,138,58,55,196,47,26,120,145,244,95,3,235,9,118,17,245,221,2,205,117,221,3,80,179,211,245,120,96,215,180,219,61,79,194,94,34,95,216,144,79,70,175,29,122,255,0,151,144,189,165,237,115,240,55,94,248,255,0,99,119,251,33,255,0,193,104,244,209,119,44,143,227,101,248,209,176,135,136,172,175,121,255,0,4,208,253,156,60,48,217,62,96,202,121,250,99,168,32,31,149,71,83,145,95,178,255,0,240,213,122,119,63,233,147,30,123,52,62,222,183,93,58,14,127,74,254,36,126,53,252,115,241,247,236,211,101,251,118,254,202,223,180,63,133,181,159,133,31,27,245,45,75,197,254,25,241,119,195,175,26,205,109,166,120,143,77,213,117,95,217,175,225,175,128,236,204,81,91,92,92,90,248,143,71,188,183,210,109,174,180,173,95,74,187,190,209,117,221,62,254,211,84,208,245,45,75,73,188,178,191,184,251,79,70,253,177,254,34,120,143,224,223,139,63,104,207,15,124,54,248,139,174,254,207,158,1,241,5,191,132,252,115,241,223,69,240,183,138,53,79,131,126,10,241,77,229,199,134,173,109,60,53,226,223,137,246,58,27,232,158,29,241,12,215,62,52,240,116,113,217,222,95,67,114,239,226,205,53,22,54,55,246,162,92,49,25,109,73,69,175,103,119,205,127,252,146,11,244,41,79,93,37,208,252,127,255,0,130,224,248,177,60,109,255,0,5,68,253,167,188,79,12,141,36,90,159,252,41,93,142,219,75,48,178,253,158,126,19,105,231,36,49,239,104,71,83,210,138,249,3,246,204,248,167,167,124,106,253,164,254,36,124,76,211,174,97,189,178,241,41,240,135,147,117,109,114,151,112,203,253,141,224,63,11,248,126,66,151,17,156,75,137,116,169,20,227,161,82,15,34,138,250,42,16,80,161,70,46,41,56,194,43,238,73,24,189,91,125,207,247,86,162,138,43,97,5,20,81,64,5,20,81,64,5,20,81,64,5,20,81,64,5,20,81,64,5,20,81,64,5,20,81,64,5,127,63,191,183,74,111,253,177,126,61,182,113,228,203,255,0,6,186,73,140,103,118,239,248,46,135,237,61,14,222,188,127,173,207,127,187,143,112,81,73,164,244,126,79,238,119,64,127,64,84,81,69,48,63,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 4777; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

