#ifndef GETVESUITE_Hetran_Hetran_H-HT-2_H
#define GETVESUITE_Hetran_Hetran_H-HT-2_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Hetran_Hetran_H-HT-2( void )
{
    unsigned char osgData[ 6100 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,65,0,142,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,205,127,105,47,219,107,225,79,236,172,62,42,159,138,58,7,196,22,255,0,133,99,251,34,124,108,253,180,108,255,0,225,27,210,188,57,171,255,0,194,204,248,93,251,55,73,165,199,241,227,64,240,15,157,226,187,127,178,248,251,195,71,198,127,8,12,246,190,36,255,0,132,119,78,213,7,198,157,23,254,17,237,87,88,58,111,140,7,133,254,195,175,243,124,240,167,237,193,255,0,5,38,253,165,190,4,248,131,94,253,163,126,35,120,23,246,142,213,255,0,105,47,216,167,226,87,236,239,166,120,207,199,255,0,12,252,49,224,143,17,124,24,248,61,251,96,120,71,192,247,255,0,28,180,239,133,118,95,179,230,145,224,189,26,243,90,214,239,60,17,240,178,236,95,120,195,74,241,116,186,100,255,0,12,180,228,209,197,133,165,247,136,237,181,222,143,194,95,240,83,207,248,46,143,137,188,65,241,71,69,255,0,135,131,120,154,63,248,87,190,58,211,252,30,175,23,236,227,251,21,7,152,94,252,52,248,119,227,239,50,227,119,236,176,219,230,7,199,5,1,85,65,229,194,131,97,96,210,63,34,197,210,231,168,165,81,40,198,214,222,253,157,254,101,114,182,149,145,254,140,20,87,243,245,255,0,4,3,255,0,130,182,248,143,254,10,51,251,56,105,254,7,253,166,53,159,4,105,191,182,191,195,47,11,120,119,93,241,70,157,162,167,252,35,215,191,29,254,14,234,158,28,240,109,206,135,251,73,232,222,14,143,73,182,211,180,169,155,196,158,38,155,195,254,46,211,116,9,238,236,180,141,119,78,178,214,36,176,240,174,143,227,127,9,104,49,255,0,64,181,212,154,106,233,221,50,66,191,0,127,104,207,28,126,195,223,181,119,198,207,216,195,246,155,253,171,62,14,124,63,241,199,236,203,224,127,248,38,7,252,21,59,246,144,241,239,128,255,0,105,239,130,222,18,248,183,172,124,2,214,62,18,124,94,255,0,130,101,199,241,111,71,248,137,240,146,77,43,197,31,240,140,254,208,31,15,254,201,241,55,194,222,42,209,52,248,111,181,221,15,94,209,60,65,225,157,178,94,199,115,109,39,239,245,127,31,223,240,81,47,130,31,29,116,111,248,43,191,194,43,127,131,126,39,255,0,132,47,246,70,248,75,241,19,254,9,229,251,81,252,106,208,228,209,60,31,173,90,234,159,20,255,0,224,162,31,240,88,143,216,163,225,150,175,240,3,193,26,101,214,173,103,55,194,223,135,254,32,248,155,255,0,4,248,177,248,231,121,171,104,26,53,230,165,119,241,30,47,137,23,30,39,214,167,79,137,208,91,90,18,146,138,188,157,150,139,239,118,95,123,118,3,250,1,255,0,135,79,127,193,44,185,255,0,141,105,254,192,3,254,236,223,246,117,231,159,251,39,52,191,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,215,223,196,129,215,215,29,59,255,0,146,105,104,184,31,0,127,195,167,127,224,150,95,244,141,63,216,3,255,0,16,223,246,117,255,0,231,115,71,252,58,119,254,9,101,255,0,72,211,253,128,63,241,13,255,0,103,95,215,254,45,207,53,247,253,20,93,119,3,224,15,248,116,239,252,18,203,254,145,167,251,0,127,226,27,254,206,191,252,238,104,255,0,135,78,255,0,193,44,191,233,26,127,176,7,254,33,183,236,235,255,0,206,226,190,255,0,162,139,174,224,127,60,63,180,255,0,236,195,255,0,4,219,240,90,95,90,252,9,253,131,255,0,102,15,128,31,180,31,236,189,255,0,5,31,255,0,130,60,233,177,120,199,194,223,178,247,192,127,133,95,16,174,188,43,241,131,246,243,253,138,53,141,31,226,223,194,63,16,248,59,195,241,107,115,252,31,214,173,124,71,241,63,192,159,219,224,105,246,218,151,137,190,16,124,67,240,188,75,116,190,31,212,25,255,0,119,190,26,124,82,240,39,198,15,14,106,94,44,248,117,174,255,0,194,69,225,253,35,226,7,197,159,133,186,134,161,253,153,172,105,31,103,241,223,192,239,138,126,50,248,41,241,75,66,251,46,187,167,218,207,47,246,95,196,223,135,254,46,211,62,212,145,181,157,239,246,71,219,116,235,139,189,62,226,214,238,111,230,123,254,14,106,209,255,0,104,111,130,223,6,188,25,251,89,126,200,94,47,241,13,183,199,95,17,120,147,225,175,195,61,103,193,243,90,252,37,135,225,238,141,240,251,246,45,185,248,179,255,0,5,100,183,253,160,238,44,252,67,225,139,91,207,136,159,23,188,15,173,126,198,247,159,240,143,89,120,183,90,241,63,134,116,207,12,248,203,199,250,55,132,252,17,23,136,254,32,107,87,218,167,244,195,240,159,225,111,129,62,7,124,44,248,105,240,83,225,110,133,255,0,8,191,195,47,131,255,0,15,252,27,240,183,225,215,134,127,180,245,141,111,254,17,223,2,124,63,240,230,155,225,63,8,232,95,219,62,34,212,46,245,13,95,236,158,31,210,52,251,127,181,95,93,221,94,92,125,159,205,186,184,154,119,121,24,190,173,117,64,122,5,20,87,159,248,203,226,151,129,62,31,248,143,225,63,132,252,93,174,255,0,100,120,131,227,143,196,13,75,225,111,194,221,63,251,51,88,191,255,0,132,163,199,122,71,194,207,137,127,26,245,13,11,237,90,102,159,52,26,39,151,240,203,224,255,0,196,93,79,237,90,140,150,150,111,255,0,8,239,216,146,225,181,11,187,27,75,166,7,160,81,69,20,1,252,74,254,195,63,179,20,58,255,0,236,79,251,30,235,198,60,157,107,246,90,253,159,117,98,126,205,187,39,82,248,77,225,43,211,243,121,195,119,250,243,218,151,246,125,253,152,225,212,126,45,126,220,182,70,51,141,7,246,165,240,150,146,0,182,206,4,223,177,63,236,121,174,253,223,63,143,155,90,60,103,190,123,224,126,218,255,0,193,48,126,12,88,106,159,240,77,95,248,39,150,164,234,165,245,31,216,111,246,75,190,115,152,134,94,243,224,31,128,46,27,170,28,115,33,235,71,236,151,240,103,79,188,248,247,255,0,5,59,183,33,118,233,127,183,31,128,44,80,19,23,220,147,254,9,171,255,0,4,243,212,184,6,63,239,234,79,211,215,215,53,249,255,0,214,191,125,141,78,87,228,154,95,249,85,35,175,150,41,69,95,117,250,31,199,103,236,181,251,25,88,124,91,248,13,251,56,95,79,165,121,205,123,251,56,252,41,241,14,233,33,158,96,86,255,0,192,222,14,249,163,242,10,176,13,44,210,185,227,102,101,224,177,57,175,164,255,0,225,217,122,71,253,1,35,255,0,192,45,75,255,0,142,87,236,175,252,17,231,224,28,30,46,253,151,255,0,100,59,233,108,99,186,138,247,246,25,248,7,169,190,248,78,214,158,227,225,199,194,210,165,131,50,6,43,19,141,164,115,137,27,146,73,199,233,183,198,175,10,126,206,127,179,111,133,236,60,115,251,68,252,83,248,47,240,19,193,58,174,191,107,225,77,43,198,31,26,190,34,120,39,225,103,133,181,47,20,223,233,218,174,175,99,225,171,15,16,248,227,196,54,54,151,126,32,155,73,208,181,187,168,172,227,153,174,36,182,209,238,231,72,204,118,211,50,221,108,202,172,113,50,163,73,185,73,54,146,90,183,175,68,181,232,40,195,221,87,124,186,35,249,52,255,0,135,101,233,31,244,4,143,255,0,0,181,47,254,57,94,147,166,126,198,31,18,116,95,134,94,34,248,41,163,120,239,226,46,147,240,111,197,250,196,30,34,241,103,194,77,51,197,190,58,176,248,101,226,143,16,90,205,161,92,91,107,190,34,240,21,174,176,154,86,181,172,199,63,133,252,50,233,117,115,105,44,232,254,29,177,101,112,214,150,230,63,234,15,89,240,159,236,231,225,207,140,158,19,253,157,124,65,241,75,224,206,133,251,65,120,247,195,247,62,44,240,47,192,173,103,226,23,130,180,207,140,158,52,240,181,164,30,37,186,187,241,47,132,254,24,222,248,134,61,111,196,122,4,86,222,11,241,132,146,222,89,216,205,111,28,126,20,212,157,164,11,99,116,98,231,244,79,248,100,191,18,255,0,194,242,255,0,132,115,227,199,236,241,226,15,248,102,15,237,127,248,105,95,236,79,139,127,14,117,83,251,60,255,0,194,63,255,0,9,71,246,241,248,229,246,15,21,73,255,0,10,144,89,127,194,15,227,79,181,255,0,111,255,0,103,253,155,254,17,29,83,206,217,253,159,119,229,47,175,99,116,253,221,75,122,63,37,127,191,250,184,249,99,252,201,216,254,84,255,0,225,217,122,71,253,1,35,255,0,192,45,75,255,0,142,81,255,0,14,203,210,63,232,9,31,254,1,106,95,252,114,191,168,221,103,197,159,177,15,135,126,13,248,79,246,138,241,7,237,57,251,45,104,95,179,239,143,124,65,115,225,47,3,124,117,214,126,58,124,40,210,254,13,248,207,197,86,83,248,150,214,243,195,94,19,248,159,123,227,36,209,60,71,175,197,115,224,207,24,71,37,149,157,244,215,49,201,225,77,77,30,48,214,23,66,47,125,183,253,159,124,11,119,226,173,99,192,182,183,62,24,185,241,183,135,116,15,13,120,179,196,30,14,183,212,116,201,252,83,161,120,91,198,122,143,139,52,143,7,120,151,88,240,252,90,129,187,211,52,13,91,86,240,23,142,109,116,219,201,225,75,123,235,143,6,106,176,218,201,44,154,117,226,195,50,199,227,32,185,165,9,193,43,221,180,210,233,187,125,111,184,40,46,146,71,241,235,255,0,14,203,210,63,232,9,31,254,1,106,95,252,114,143,248,118,94,145,255,0,64,72,255,0,240,11,82,255,0,227,149,253,148,255,0,195,41,105,223,244,4,182,255,0,191,9,255,0,199,232,255,0,134,82,211,191,232,7,109,255,0,126,23,255,0,143,214,63,219,21,52,106,111,79,248,31,168,253,159,153,252,107,127,195,178,244,143,250,2,71,255,0,128,90,151,255,0,28,163,254,29,151,164,127,208,18,63,252,2,212,191,248,229,127,89,127,26,188,41,251,57,254,205,190,23,176,241,207,237,19,241,79,224,191,192,79,4,234,186,253,175,133,52,175,24,124,106,248,137,224,159,133,158,22,212,188,83,127,167,106,186,189,143,134,172,60,67,227,143,16,216,218,93,248,130,109,39,66,214,238,162,179,142,102,184,146,219,71,187,157,35,49,219,76,202,186,207,132,255,0,103,63,14,124,100,240,159,236,235,226,15,138,95,6,116,47,218,11,199,190,31,185,241,103,129,126,5,107,63,16,188,21,166,124,100,241,167,133,173,32,241,45,213,223,137,124,39,240,198,247,196,49,235,126,35,208,34,182,240,95,140,36,150,242,206,198,107,120,227,240,166,164,237,32,91,27,163,22,208,204,49,115,140,103,8,206,112,125,82,109,59,90,250,237,160,156,18,122,202,199,242,201,224,255,0,216,63,197,63,15,127,225,41,62,0,215,252,91,224,127,248,78,60,29,174,124,59,241,175,252,33,250,215,139,124,51,255,0,9,135,195,255,0,19,125,151,254,18,79,3,120,167,251,23,81,131,254,18,15,7,234,31,98,178,251,118,153,119,231,89,93,253,146,47,180,65,39,150,152,227,191,225,217,122,71,253,1,35,255,0,192,45,75,255,0,142,87,245,89,162,127,195,37,248,151,254,23,151,252,35,159,30,63,103,143,16,127,195,48,127,107,255,0,195,74,255,0,98,124,91,248,115,170,159,217,231,254,17,255,0,248,74,63,183,143,199,47,176,120,170,79,248,84,130,203,254,16,127,26,125,175,251,127,251,63,236,223,240,136,234,158,118,207,236,251,191,43,3,89,241,103,236,67,225,223,131,126,19,253,162,188,65,251,78,254,203,90,23,236,253,227,207,16,92,248,79,192,223,29,117,159,142,159,9,244,207,131,126,51,241,77,156,254,37,181,188,240,223,132,254,39,222,248,201,52,79,17,235,241,92,248,51,198,17,201,103,103,125,53,204,114,120,83,83,71,140,53,133,208,138,190,187,143,109,126,234,165,244,95,11,235,107,126,95,52,28,171,249,209,252,185,127,195,178,244,158,218,36,121,237,254,133,169,117,255,0,191,149,225,31,23,191,224,158,58,94,131,241,7,246,89,210,151,72,141,87,198,191,30,60,65,225,169,23,236,151,227,205,75,79,217,139,246,141,241,146,161,12,249,108,73,225,36,108,47,63,186,207,221,13,95,220,13,191,236,251,224,91,191,21,107,30,5,181,185,240,189,207,141,252,59,160,120,107,197,158,32,240,117,190,161,166,79,226,157,11,194,222,51,212,124,89,164,120,59,196,186,207,135,163,212,13,222,153,225,253,91,86,240,23,142,109,116,219,201,225,75,107,235,143,6,106,176,219,73,44,154,117,226,195,241,191,237,119,251,55,88,104,191,27,63,224,154,209,127,101,219,192,53,175,219,91,226,14,150,204,32,70,7,202,255,0,130,110,255,0,193,65,117,184,195,13,238,54,172,186,52,111,200,218,60,189,204,48,49,85,71,51,173,237,163,78,163,148,101,105,59,59,244,139,123,124,191,46,226,149,59,165,102,158,171,243,63,17,190,21,126,217,63,240,82,15,216,143,199,250,79,142,159,227,143,197,127,218,55,192,13,170,232,247,159,18,126,16,252,121,248,129,174,124,75,211,60,109,160,233,246,218,229,164,186,31,134,252,115,227,107,125,91,85,248,87,173,136,53,217,46,45,175,116,25,109,162,184,212,237,180,169,117,203,31,16,216,105,227,68,185,254,185,255,0,96,255,0,219,39,193,31,183,127,236,211,224,159,218,19,193,218,65,240,133,254,175,54,171,225,191,136,127,13,47,60,73,161,120,159,93,248,89,241,39,195,55,95,98,241,47,131,53,219,253,10,110,85,163,125,59,86,209,230,189,181,210,239,245,47,13,248,167,68,214,174,116,141,44,234,107,101,15,226,119,237,105,240,47,71,179,211,117,136,86,214,21,141,81,240,179,249,36,200,137,27,130,172,201,20,187,217,86,6,93,196,163,102,53,42,113,95,140,255,0,177,119,237,221,240,207,254,9,49,251,100,124,78,248,179,241,134,195,226,206,181,240,115,226,151,193,221,95,225,246,165,224,47,130,95,217,122,212,218,183,196,203,127,30,120,63,196,190,5,241,111,136,188,47,227,79,28,120,123,74,187,135,73,208,52,223,139,118,182,151,198,230,123,235,3,227,251,155,123,56,5,190,167,168,186,123,121,86,101,42,239,146,164,185,147,234,253,17,149,72,37,126,141,31,191,223,240,77,79,142,173,164,127,193,57,127,96,29,39,237,49,175,246,95,236,79,251,42,233,219,75,54,71,216,190,5,120,14,219,4,121,131,145,229,122,83,191,101,111,142,173,103,241,211,254,10,85,115,246,168,135,246,191,237,177,224,61,68,146,205,134,242,255,0,224,156,223,176,22,147,185,79,152,50,63,226,89,143,109,188,215,224,223,236,105,251,81,77,225,255,0,217,3,246,83,208,87,86,142,33,162,126,205,191,3,52,128,132,195,242,127,102,252,48,240,181,152,143,230,143,60,8,113,223,165,47,192,207,218,134,125,51,226,127,237,149,122,53,104,208,248,131,246,146,240,182,174,91,116,63,188,104,63,100,15,217,79,65,243,7,238,176,64,26,34,174,120,255,0,87,236,107,198,250,155,246,184,199,203,103,57,167,183,253,61,76,213,78,234,58,109,254,71,233,63,252,18,59,246,131,131,193,159,178,175,236,143,97,53,233,133,109,127,98,127,128,218,120,242,124,204,22,182,248,115,240,210,6,71,8,84,151,85,129,119,114,71,221,201,228,99,245,203,254,26,230,195,254,130,179,254,87,31,252,126,191,136,15,217,19,246,136,241,211,252,43,253,150,190,26,124,42,240,135,141,126,37,252,68,213,62,2,124,52,240,215,135,252,7,240,255,0,195,26,199,142,60,103,173,55,134,190,21,233,26,198,179,30,139,225,63,15,90,207,123,169,37,174,129,225,187,139,233,204,48,73,228,218,89,79,112,197,32,142,87,79,188,127,182,127,224,161,95,244,99,63,182,143,254,34,151,198,159,254,100,105,214,202,106,214,171,58,145,131,146,114,122,175,82,99,82,201,43,167,234,127,81,223,240,215,54,31,244,20,159,254,249,159,255,0,143,127,156,81,255,0,13,113,97,255,0,65,73,255,0,239,153,255,0,248,253,127,46,63,219,63,240,80,175,250,49,159,219,71,255,0,17,75,227,79,255,0,50,52,127,108,255,0,193,66,127,232,198,191,109,31,252,69,47,141,63,252,200,214,127,216,213,255,0,231,219,187,255,0,128,87,181,243,95,215,204,254,163,191,225,174,44,63,232,41,63,253,243,63,255,0,31,163,254,26,226,195,254,130,147,254,87,31,252,126,191,151,31,237,159,248,40,87,253,24,207,237,163,255,0,136,165,241,167,255,0,153,26,63,182,127,224,161,95,244,99,63,182,143,254,34,151,198,159,254,100,104,254,198,174,255,0,229,219,254,172,30,215,205,127,95,51,250,142,255,0,134,184,176,255,0,160,164,255,0,247,204,255,0,252,126,143,248,107,141,63,254,130,147,255,0,223,51,255,0,241,250,254,92,127,182,127,224,161,95,244,99,63,182,143,254,34,151,198,159,254,100,104,254,217,255,0,130,133,127,209,140,254,218,63,248,138,95,26,127,249,145,165,253,139,89,255,0,203,167,167,144,123,87,221,127,95,51,250,142,255,0,134,185,176,255,0,160,172,255,0,149,199,255,0,31,163,254,26,230,195,254,130,147,255,0,223,51,255,0,241,239,243,138,254,92,127,182,127,224,161,95,244,99,63,182,143,254,34,151,198,159,254,100,104,254,217,255,0,130,133,127,209,140,254,218,63,248,138,95,26,127,249,145,163,251,26,186,191,238,154,254,144,123,95,53,253,124,207,234,59,254,26,226,195,254,130,147,255,0,223,51,255,0,241,250,63,225,174,44,63,232,41,63,253,243,63,255,0,31,175,229,195,251,103,254,10,21,255,0,70,51,251,104,145,207,252,218,151,198,145,158,56,255,0,153,75,215,249,126,52,191,219,63,240,80,159,250,49,159,219,71,191,252,218,151,198,161,223,254,197,30,152,252,243,219,20,255,0,177,171,255,0,207,183,248,135,181,243,95,215,204,254,163,191,225,174,108,63,232,41,57,252,39,253,63,127,95,18,254,214,31,180,221,142,179,241,171,254,9,195,58,223,188,159,216,127,182,119,143,181,95,222,172,187,80,207,255,0,4,234,253,190,116,24,165,249,153,240,4,218,220,99,59,73,27,178,1,198,15,226,95,246,207,252,20,43,254,140,103,246,209,255,0,196,82,248,211,255,0,204,141,124,251,241,155,86,253,186,27,226,55,236,150,218,223,236,109,251,91,233,58,146,126,208,94,35,127,8,233,250,175,236,209,241,115,79,190,241,71,136,79,236,169,251,76,71,115,161,120,106,206,235,194,233,38,191,173,71,225,55,241,62,166,246,150,194,73,163,211,188,61,127,124,232,45,172,174,36,77,104,101,21,161,85,73,211,110,202,75,85,222,45,126,160,234,233,186,254,153,251,123,251,86,126,209,122,109,246,149,171,207,21,250,20,144,109,192,152,92,145,230,32,9,26,23,142,96,243,21,184,218,66,21,82,211,177,44,24,0,62,2,255,0,130,93,126,194,255,0,14,191,224,165,159,181,103,198,63,16,254,210,191,4,117,223,139,159,178,55,195,63,134,218,254,128,190,35,185,241,23,138,190,29,120,118,215,246,144,191,241,103,195,141,67,194,186,14,153,175,252,59,241,110,139,170,120,143,93,131,225,180,255,0,17,111,47,236,160,154,234,203,77,180,241,38,145,117,172,195,107,46,173,225,183,186,246,255,0,131,63,240,70,239,248,40,151,237,37,241,15,71,176,253,170,116,235,15,217,127,224,198,149,170,105,43,227,237,73,126,37,120,3,199,159,21,60,95,225,155,139,93,126,109,67,77,248,77,167,124,58,188,241,14,147,101,173,25,172,180,235,11,171,223,17,221,105,208,233,209,248,150,219,84,179,178,241,48,211,238,244,57,191,171,191,217,115,246,108,248,105,251,32,252,2,248,109,251,56,252,33,143,95,30,1,248,99,164,94,233,250,93,223,138,245,185,188,67,226,125,111,82,214,245,189,83,197,94,42,241,63,136,117,89,35,138,57,117,189,91,197,218,238,187,169,221,69,103,111,101,166,91,77,171,61,182,149,167,105,218,100,54,150,54,254,222,89,150,60,61,167,86,54,107,167,221,185,148,230,157,213,238,217,252,6,120,183,254,8,193,251,99,254,197,127,10,62,36,120,119,246,149,253,166,126,19,232,158,40,248,53,251,2,126,211,95,181,175,193,205,7,224,6,157,227,239,139,254,21,248,139,224,207,216,99,195,223,10,180,239,137,94,10,248,131,226,175,136,86,223,15,110,190,16,235,183,250,151,198,95,132,144,248,126,109,51,68,241,157,181,205,181,199,136,174,117,31,236,169,116,109,54,207,196,94,201,162,127,193,180,63,240,89,157,15,82,241,126,167,111,241,187,246,6,105,188,99,226,43,111,17,223,129,241,219,246,150,143,101,221,175,132,252,47,225,20,82,195,246,71,62,121,251,15,133,172,142,242,20,128,251,49,132,5,191,184,207,141,127,178,215,192,143,218,43,251,96,124,99,240,32,241,136,215,254,0,252,123,253,151,117,96,124,77,227,13,3,237,127,2,255,0,105,230,248,110,223,28,124,14,63,225,23,241,5,151,217,206,182,223,8,254,30,17,170,69,179,89,211,15,135,179,163,234,26,127,218,175,126,211,244,5,122,203,15,79,154,114,113,77,74,214,86,218,218,189,122,221,164,204,249,157,173,115,241,47,254,8,189,255,0,4,108,248,125,255,0,4,179,248,45,164,94,120,199,87,209,190,47,126,216,190,47,240,46,131,225,111,139,159,26,173,109,174,155,65,240,199,135,180,251,109,26,68,248,41,240,50,13,94,214,43,175,15,252,39,181,212,52,77,34,125,70,254,104,45,53,127,27,234,218,45,174,185,175,65,101,101,167,120,87,194,254,18,253,180,162,138,221,43,104,132,21,248,121,251,83,124,25,248,169,241,35,254,10,25,7,128,244,232,62,31,233,195,246,134,240,31,236,59,241,151,194,58,165,239,139,252,70,227,73,248,89,255,0,4,97,255,0,130,149,254,203,255,0,180,151,196,91,159,18,88,193,240,252,253,159,226,7,141,191,225,190,190,41,104,186,38,145,107,53,222,157,166,127,195,62,232,26,141,247,136,102,255,0,133,129,168,216,124,61,253,195,175,128,62,35,127,202,83,127,99,127,251,48,15,248,41,103,254,180,87,252,18,118,148,162,164,173,37,116,154,127,52,211,95,138,64,125,255,0,69,20,83,0,162,138,40,0,162,138,40,3,240,251,254,11,107,240,107,226,151,199,95,130,90,127,132,224,182,240,7,135,62,24,194,250,191,195,47,1,120,226,111,24,120,139,83,241,223,136,191,106,143,248,40,23,194,127,142,159,240,73,255,0,132,94,3,241,127,195,68,248,123,14,159,224,191,217,251,73,241,23,237,237,225,47,27,120,143,199,182,94,43,215,252,71,5,151,195,253,71,66,211,62,25,106,115,221,219,106,139,251,131,95,0,127,193,75,63,228,221,126,28,255,0,217,255,0,255,0,193,39,127,245,233,191,177,189,125,255,0,69,150,253,64,43,199,254,37,252,20,240,175,197,79,26,126,207,94,58,241,14,161,226,11,61,95,246,107,248,193,173,252,107,240,45,190,141,117,167,91,233,218,183,138,181,239,128,95,28,63,103,59,205,63,197,144,223,105,87,18,223,120,125,124,19,241,251,198,55,81,197,103,53,133,200,213,116,221,50,118,187,123,72,110,172,111,125,130,138,0,40,162,138,0,40,162,138,0,40,162,138,0,43,248,3,255,0,131,231,63,231,23,95,247,123,63,251,232,244,81,64,31,192,29,20,81,64,5,20,81,64,5,20,81,64,31,127,255,0,193,39,127,229,41,191,240,77,63,251,63,255,0,216,223,255,0,90,43,225,205,127,183,205,20,80,1,69,20,80,1,69,20,80,7,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 6100; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

