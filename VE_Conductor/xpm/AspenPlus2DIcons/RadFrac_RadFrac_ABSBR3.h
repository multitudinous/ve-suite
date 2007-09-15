#ifndef GETVESUITE_RadFrac_RadFrac_ABSBR3_H
#define GETVESUITE_RadFrac_RadFrac_ABSBR3_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_RadFrac_RadFrac_ABSBR3( void )
{
    unsigned char osgData[ 4069 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,143,0,49,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,216,63,107,159,218,231,224,143,236,71,240,71,196,127,30,190,61,120,146,77,23,194,154,44,176,233,58,22,135,165,67,111,168,248,215,226,63,141,181,27,123,201,252,61,240,227,225,199,135,167,188,131,254,18,63,27,106,63,96,189,104,97,105,173,237,44,237,52,251,221,95,87,189,211,116,61,55,83,212,236,191,143,255,0,141,255,0,240,89,63,248,41,87,237,129,168,222,105,255,0,8,46,116,175,216,219,225,38,171,164,106,250,27,120,87,225,131,88,120,211,226,126,181,97,226,63,15,104,154,110,174,190,32,248,239,226,191,10,71,125,165,107,86,247,246,62,39,151,67,212,252,17,163,248,43,83,209,78,190,99,150,251,81,212,52,251,13,86,47,150,191,110,143,218,99,196,223,240,80,175,248,40,23,197,175,22,106,94,36,155,196,255,0,3,126,14,120,243,197,223,7,255,0,102,221,11,69,241,13,183,140,254,28,233,254,4,240,166,169,7,134,53,175,137,94,3,215,60,53,109,101,99,172,15,30,235,126,24,255,0,132,166,93,85,82,242,237,236,53,109,31,70,254,217,213,52,159,15,232,178,197,250,1,251,56,124,14,210,30,218,198,56,236,155,105,72,23,15,100,241,130,83,10,155,86,52,196,113,162,164,101,21,112,62,232,225,16,164,158,30,97,153,58,45,194,155,178,142,237,110,223,233,253,92,210,17,79,86,181,63,62,188,67,251,63,254,210,95,180,6,153,7,133,62,61,126,208,191,180,79,199,143,6,105,247,80,120,159,73,240,127,198,79,140,255,0,20,190,42,120,102,199,196,86,246,247,26,109,151,137,244,255,0,15,120,243,197,90,181,149,142,187,6,147,172,107,22,177,222,71,111,29,212,112,107,119,48,71,48,134,226,225,30,183,133,255,0,101,143,141,255,0,179,210,223,159,217,227,227,7,198,207,217,248,248,185,173,7,139,215,224,159,196,255,0,136,31,9,27,197,195,65,251,79,252,35,237,226,83,240,251,94,209,199,136,23,79,58,214,180,44,254,216,110,13,167,246,229,208,182,17,139,153,252,207,234,39,225,191,236,225,167,94,69,108,176,90,192,132,52,81,152,228,134,38,38,222,71,142,73,21,99,96,68,35,112,202,157,155,130,184,237,154,222,248,131,251,53,88,216,67,114,178,218,196,138,171,229,195,20,112,195,28,202,178,164,66,225,119,5,95,53,206,210,0,101,192,223,133,32,230,190,127,251,98,175,180,94,251,118,243,54,246,94,72,254,121,62,13,255,0,193,80,191,224,167,255,0,177,142,175,160,218,107,31,19,110,63,106,127,131,254,30,105,173,53,47,134,223,180,33,62,36,241,53,230,135,168,120,190,207,196,122,222,167,166,124,125,75,47,248,77,109,252,107,38,157,15,136,173,52,203,255,0,17,234,94,47,209,52,91,61,111,202,30,28,190,181,211,244,187,107,79,235,35,254,9,235,255,0,5,10,248,53,255,0,5,16,248,52,126,34,124,60,18,120,71,226,31,132,100,211,116,63,141,223,4,117,205,74,11,239,24,124,34,241,141,244,23,50,91,219,92,220,71,109,111,255,0,9,55,129,53,63,236,253,74,127,13,248,146,11,107,123,93,102,214,194,230,41,109,180,205,115,76,215,116,45,31,249,247,253,161,254,6,105,48,91,223,199,246,34,168,12,140,4,22,178,101,89,71,201,34,124,160,163,2,168,160,14,48,170,203,176,163,121,191,138,250,119,198,79,136,159,240,79,143,218,95,192,31,181,31,193,102,215,237,53,63,1,107,118,81,120,227,194,122,29,195,232,54,223,23,126,20,77,172,233,151,222,62,248,61,226,91,187,173,51,82,180,77,3,92,210,244,209,12,83,205,165,95,255,0,98,234,86,250,110,189,166,91,71,171,104,218,100,246,254,230,93,153,202,171,81,170,238,159,125,214,221,126,102,82,134,186,104,207,244,167,162,190,40,255,0,135,149,127,193,57,191,232,255,0,127,98,143,252,74,159,129,127,252,221,209,94,253,215,116,100,127,156,167,252,19,134,121,238,190,29,252,57,184,185,154,91,139,139,159,10,248,110,107,137,231,145,230,158,121,166,210,172,164,150,105,165,145,139,75,43,72,204,204,204,73,37,137,39,53,253,80,254,205,159,234,116,255,0,164,63,250,2,215,242,179,255,0,4,218,255,0,146,107,240,203,254,197,15,11,255,0,233,154,194,191,170,111,217,179,253,70,157,244,139,255,0,64,90,248,204,209,39,86,162,233,119,250,29,16,251,63,35,246,235,224,175,75,63,247,35,255,0,31,231,82,252,105,158,117,91,176,179,74,161,99,112,160,72,224,0,6,6,48,220,113,81,124,21,233,103,254,228,127,202,143,141,93,47,63,220,147,249,87,204,199,248,191,119,232,116,31,137,191,180,149,221,216,131,81,197,213,192,226,81,196,242,244,10,78,62,255,0,175,242,175,230,27,246,235,212,181,24,244,173,116,199,127,122,132,65,115,130,151,83,169,31,35,158,10,201,199,32,126,85,253,56,254,210,127,234,53,31,164,191,250,3,87,243,3,251,119,127,200,39,94,255,0,174,23,63,250,4,149,244,185,95,199,31,95,254,68,198,127,19,63,148,239,248,78,60,105,255,0,67,127,138,63,240,127,171,127,242,93,21,203,209,95,105,101,217,28,167,245,99,255,0,4,218,255,0,146,107,240,203,254,197,15,11,255,0,233,154,194,191,170,111,217,179,253,70,157,244,139,255,0,64,90,254,86,127,224,155,95,242,77,126,25,127,216,161,225,127,253,51,88,87,245,77,251,54,127,168,211,190,145,127,232,11,95,27,153,255,0,26,167,248,159,232,116,67,236,252,143,219,175,130,189,44,255,0,220,143,249,81,241,171,165,231,251,146,127,42,62,10,244,179,255,0,114,63,229,71,198,174,151,159,238,73,252,171,230,35,252,95,187,244,58,15,196,95,218,79,253,70,163,244,151,255,0,64,106,254,96,127,110,239,249,4,235,223,245,194,231,255,0,64,146,191,167,239,218,79,253,70,163,244,151,255,0,64,106,254,96,127,110,239,249,4,235,223,245,194,231,255,0,64,146,190,151,43,248,225,235,255,0,200,152,207,226,103,242,77,69,20,87,218,156,167,245,99,255,0,4,218,255,0,146,107,240,203,254,197,15,11,255,0,233,154,194,191,170,111,217,179,253,70,157,244,139,255,0,64,90,254,86,127,224,155,95,242,77,126,25,127,216,161,225,127,253,51,88,87,245,77,251,54,127,168,211,190,145,127,232,11,95,25,153,255,0,26,167,248,159,232,116,67,236,252,143,219,175,130,189,44,255,0,220,143,249,81,241,171,165,231,251,146,127,42,62,10,244,179,255,0,114,63,229,71,198,174,151,159,238,73,252,171,230,35,252,95,187,244,58,15,196,95,218,79,253,70,163,244,151,255,0,64,106,254,96,127,110,239,249,4,235,223,245,194,231,255,0,64,146,191,167,239,218,79,253,70,163,244,151,255,0,64,106,254,96,127,110,239,249,4,235,223,245,194,231,255,0,64,146,190,151,43,248,225,235,255,0,200,152,207,226,103,242,77,69,20,87,218,156,167,245,99,255,0,4,218,255,0,146,107,240,203,254,197,15,11,255,0,233,154,194,191,170,111,217,179,253,70,157,244,139,255,0,64,90,254,86,127,224,155,95,242,77,126,25,127,216,161,225,127,253,51,88,87,245,77,251,54,127,168,211,190,145,127,232,11,95,25,153,255,0,26,167,248,159,232,116,67,236,252,143,219,175,130,189,44,255,0,220,143,249,81,241,171,165,231,251,146,127,42,62,10,244,179,255,0,114,63,229,71,198,174,151,159,238,73,252,171,230,35,252,95,187,244,58,15,196,95,218,79,253,70,163,244,151,255,0,64,106,254,96,127,110,239,249,4,235,223,245,194,231,255,0,64,146,191,167,239,218,79,253,70,163,244,151,255,0,64,106,254,96,127,110,239,249,4,235,223,245,194,231,255,0,64,146,190,151,43,248,225,235,255,0,200,152,207,226,103,242,77,69,20,87,218,156,167,245,99,255,0,4,218,255,0,146,107,240,203,254,197,15,11,255,0,233,154,194,191,170,111,217,179,253,70,157,244,139,255,0,64,90,254,86,127,224,155,95,242,77,126,25,127,216,161,225,127,253,51,88,87,245,77,251,54,127,168,211,190,145,127,232,11,95,25,153,255,0,26,167,248,159,232,116,67,236,252,143,219,175,130,189,44,255,0,220,143,249,81,241,171,165,231,251,146,127,42,62,10,244,179,255,0,114,63,229,71,198,174,151,159,238,73,252,171,230,35,252,95,187,244,58,15,196,95,218,79,253,70,163,244,151,255,0,64,106,254,96,127,110,239,249,4,235,223,245,194,231,255,0,64,146,191,167,239,218,79,253,70,163,244,151,255,0,64,106,254,96,127,110,239,249,4,235,223,245,194,231,255,0,64,146,190,151,43,248,225,235,255,0,200,152,207,226,103,242,77,69,20,87,218,156,167,245,99,255,0,4,218,255,0,146,107,240,203,254,197,15,11,255,0,233,154,194,191,170,111,217,179,253,70,157,244,139,255,0,64,90,254,86,127,224,155,95,242,77,126,25,127,216,161,225,127,253,51,88,87,245,77,251,54,127,168,211,190,145,127,232,11,95,25,153,255,0,26,167,248,159,232,116,67,236,252,143,219,175,130,189,44,255,0,220,143,249,81,241,171,165,231,251,146,127,42,62,10,244,179,255,0,114,63,229,71,198,174,151,159,238,73,252,171,230,35,252,95,187,244,58,15,196,95,218,79,253,70,163,244,151,255,0,64,106,254,96,127,110,239,249,4,235,223,245,194,231,255,0,64,146,191,167,239,218,79,253,70,163,244,151,255,0,64,106,254,96,127,110,239,249,4,235,223,245,194,231,255,0,64,146,190,151,43,248,225,235,255,0,200,152,207,226,103,242,77,69,20,87,218,156,167,245,99,255,0,4,218,255,0,146,107,240,203,254,197,15,11,255,0,233,154,194,191,170,111,217,179,253,70,157,244,139,255,0,64,90,254,88,255,0,224,157,22,87,58,111,128,190,31,105,215,177,249,55,150,30,26,240,245,149,220,33,227,151,202,185,181,211,44,224,158,63,50,39,100,147,108,168,195,42,197,78,50,9,28,215,245,59,251,54,113,14,157,159,72,189,191,128,113,207,191,242,175,140,204,255,0,141,63,87,250,29,16,251,63,35,246,231,224,175,75,63,247,35,254,84,124,106,233,121,254,228,159,202,163,248,45,44,42,109,21,229,141,72,72,242,11,160,97,242,130,62,82,123,130,15,208,213,191,140,240,72,226,235,111,150,75,163,237,95,58,29,196,158,139,179,204,206,238,122,117,230,190,98,58,84,187,209,105,250,29,7,225,247,237,39,254,163,81,250,75,255,0,160,53,127,48,63,183,119,252,130,117,239,250,225,115,255,0,160,73,95,212,119,237,37,165,223,180,58,134,219,126,190,111,252,181,132,117,12,160,243,37,127,49,95,183,71,135,181,123,141,43,91,16,218,111,47,13,200,92,207,106,185,5,88,127,20,227,31,120,87,210,229,154,78,14,250,63,248,6,50,248,153,252,134,209,94,163,255,0,10,99,226,79,253,11,131,255,0,7,26,23,255,0,44,232,175,179,231,135,243,47,189,28,167,246,235,251,118,127,193,61,60,101,255,0,4,167,253,164,181,125,78,199,195,177,234,31,178,7,198,63,136,62,32,212,191,103,159,28,104,137,175,92,233,30,8,77,94,235,84,241,12,63,179,223,143,47,252,67,174,106,23,150,62,63,208,244,133,158,61,38,239,81,190,152,120,191,68,240,251,235,22,183,103,80,183,241,54,155,225,191,175,63,103,31,218,15,69,91,77,62,88,231,182,140,48,132,40,4,54,242,25,128,216,37,228,157,225,195,171,5,100,242,155,33,94,39,141,255,0,178,143,140,255,0,6,62,22,254,208,255,0,11,124,107,240,87,227,87,130,180,111,136,127,11,254,33,232,207,161,248,183,194,90,226,79,246,77,70,207,207,130,246,206,234,214,242,202,120,110,180,77,118,199,85,180,176,191,210,245,75,9,237,181,45,39,82,211,45,53,61,50,238,210,254,210,218,230,47,228,251,246,129,255,0,131,119,191,105,159,131,186,181,247,137,63,97,175,143,90,71,197,239,0,90,104,154,214,162,126,21,124,126,212,45,252,27,241,150,43,189,23,195,222,30,58,103,135,252,39,241,19,195,30,25,30,19,248,137,175,107,90,189,191,139,99,180,58,214,155,224,29,59,71,23,90,85,181,246,171,122,141,168,107,16,121,56,252,185,215,110,116,245,111,167,245,255,0,14,105,25,218,201,233,99,244,51,225,199,237,69,167,195,13,186,65,169,197,22,17,111,2,137,12,165,161,141,225,80,170,170,251,162,15,187,112,204,101,80,109,227,133,21,185,227,239,218,150,202,226,217,214,93,78,38,23,81,200,208,194,100,49,56,251,52,112,239,143,123,50,239,46,28,253,196,220,56,7,181,127,54,191,180,39,133,255,0,224,160,127,176,119,130,116,175,139,127,181,215,192,45,87,224,255,0,195,79,16,248,158,199,225,182,133,226,57,126,38,124,27,241,212,119,158,54,213,180,157,107,196,218,102,132,186,55,195,31,137,190,33,191,182,50,104,30,15,241,52,226,226,91,40,172,208,105,158,84,151,81,205,45,188,83,243,31,179,127,136,127,108,191,248,40,24,241,153,253,140,254,15,106,191,25,191,225,82,143,14,255,0,194,199,242,60,119,240,207,225,249,240,231,252,39,191,219,159,240,135,239,255,0,133,169,227,159,13,127,107,125,179,254,16,207,20,227,236,31,109,242,63,178,143,218,190,205,231,219,249,255,0,62,242,106,222,210,220,142,239,200,219,218,249,163,244,127,246,133,253,161,180,105,237,117,9,77,197,172,170,4,174,65,104,226,37,12,110,202,228,238,2,59,127,47,46,93,136,229,8,37,22,54,50,126,88,124,38,253,146,190,47,127,193,85,190,62,201,240,27,224,149,172,58,15,134,116,135,211,117,95,141,255,0,26,53,109,46,243,81,240,95,193,175,1,234,23,87,144,13,79,83,183,138,242,216,235,158,56,212,254,197,169,91,248,95,195,113,92,195,123,172,221,233,183,19,79,46,153,162,216,107,250,198,133,250,157,240,51,254,8,31,251,115,124,127,214,124,61,226,47,218,223,226,95,132,63,102,47,134,151,146,205,169,120,139,192,94,15,214,172,62,42,254,208,33,116,239,25,88,219,92,120,77,219,68,138,95,3,248,49,245,47,12,65,175,222,233,158,34,183,241,15,141,127,179,62,219,166,38,163,225,155,201,230,212,108,244,239,234,139,246,71,253,145,254,8,254,196,159,4,124,55,240,23,224,47,134,228,209,124,41,162,201,54,173,174,107,154,180,214,250,143,141,126,35,120,215,81,183,179,131,196,63,17,190,35,248,134,11,56,63,225,35,241,174,163,246,11,37,154,101,134,222,210,206,211,79,178,210,52,139,45,55,67,211,116,205,50,203,219,203,242,183,69,243,85,90,45,82,235,211,70,140,101,59,222,219,247,63,58,127,226,29,143,248,35,151,253,25,239,254,108,15,237,75,255,0,207,186,138,253,174,162,189,190,88,255,0,42,251,140,207,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 4069; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

