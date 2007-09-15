#ifndef GETVESUITE_Distl_Distl_ICON1_H
#define GETVESUITE_Distl_Distl_ICON1_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Distl_Distl_ICON1( void )
{
    unsigned char osgData[ 8698 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,205,0,88,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,254,40,162,138,0,40,162,138,0,40,162,138,0,43,249,231,255,0,130,184,255,0,193,97,252,107,251,46,120,251,82,253,141,191,100,239,9,104,254,35,253,163,252,65,240,198,13,111,196,63,25,181,61,111,195,154,239,134,191,103,45,87,198,23,208,195,225,59,3,240,214,56,238,228,241,175,197,86,240,140,90,150,186,52,205,117,244,125,51,70,178,241,7,132,245,251,187,111,21,233,122,164,218,51,254,241,124,75,248,139,224,223,131,255,0,14,124,127,241,107,226,46,177,255,0,8,239,195,239,133,222,9,241,87,196,95,29,120,131,251,63,85,213,191,176,188,27,224,157,10,255,0,196,190,39,214,63,178,180,43,27,171,221,79,236,186,38,153,125,63,217,236,237,174,46,166,242,60,187,120,37,153,146,54,255,0,61,127,216,167,193,222,43,248,187,226,223,19,124,99,248,148,171,170,124,81,248,191,227,61,119,226,119,196,111,18,139,111,12,233,205,174,248,247,198,186,220,254,39,241,142,190,250,127,135,160,181,183,177,55,126,34,189,213,231,54,214,75,111,101,8,141,45,237,96,134,210,56,109,199,22,63,18,240,212,92,163,241,61,138,140,121,141,223,19,124,42,253,170,127,107,136,175,95,246,153,253,160,126,53,124,117,210,53,111,16,106,127,16,165,240,103,142,254,34,120,175,196,95,13,52,175,26,220,222,106,17,71,226,79,9,124,45,184,212,91,195,126,7,154,45,55,196,26,204,22,80,104,154,46,153,105,166,216,107,147,105,250,92,54,122,108,130,213,174,248,118,223,246,230,253,141,173,244,177,251,50,254,212,31,27,190,21,233,30,3,77,103,254,16,95,135,17,248,219,87,241,23,193,111,15,203,226,123,13,86,231,92,100,248,9,227,57,111,252,25,170,37,222,175,226,29,86,233,150,235,195,199,102,173,172,255,0,108,65,44,58,186,91,223,175,244,119,240,215,224,215,129,188,41,224,77,119,198,254,52,212,124,55,224,191,5,248,63,195,154,183,137,188,103,226,223,22,95,105,254,28,240,191,133,188,51,225,253,42,77,83,95,241,23,136,124,67,171,78,150,58,38,129,97,167,105,210,75,117,125,115,48,134,27,123,43,137,238,39,142,37,102,79,138,62,51,252,26,176,253,171,44,175,53,141,26,202,21,253,145,196,87,109,161,235,145,218,16,63,107,189,139,31,149,173,232,243,180,168,145,126,200,75,20,9,37,149,236,109,27,252,98,146,38,186,179,31,240,167,205,165,215,197,239,152,165,153,87,157,71,85,212,229,163,23,172,155,249,217,107,102,223,72,254,137,179,111,103,100,157,147,252,207,144,190,13,120,55,246,105,255,0,131,130,254,29,254,216,223,12,83,246,34,253,130,191,101,159,218,71,76,248,19,227,31,136,218,55,197,255,0,9,248,3,192,58,87,196,79,219,59,246,152,248,155,161,248,211,76,240,223,237,25,101,241,78,195,225,106,124,64,248,81,251,55,248,79,246,128,139,195,190,39,241,106,232,215,62,47,215,252,97,125,241,7,194,54,154,175,140,111,60,14,250,222,129,241,136,175,150,103,241,20,31,177,199,237,207,251,48,126,211,127,218,105,224,45,35,225,111,198,239,4,191,196,127,29,29,31,79,241,52,186,7,193,111,17,106,208,248,51,227,219,46,137,109,165,106,23,122,186,220,252,23,215,126,32,219,58,218,218,222,106,168,4,115,104,251,53,136,44,103,82,190,167,9,138,142,34,146,168,236,158,218,191,248,99,25,43,51,253,12,232,162,138,235,36,43,240,135,254,9,149,255,0,4,202,255,0,130,110,120,251,254,9,185,255,0,4,249,241,215,142,191,224,159,63,177,7,141,60,111,227,79,216,131,246,80,241,111,140,124,99,226,207,217,67,224,55,136,252,85,226,207,21,120,143,224,55,128,181,143,16,248,155,196,222,33,214,60,5,53,222,189,226,11,253,94,242,242,234,246,246,234,105,110,110,174,110,164,158,121,100,149,217,143,236,111,197,175,142,31,5,126,1,120,114,203,198,63,29,190,47,252,46,248,43,225,29,75,91,182,240,214,157,226,175,139,95,16,60,39,240,227,195,151,254,35,188,176,212,181,75,79,15,217,107,158,49,213,172,237,110,181,185,116,205,27,87,185,142,210,57,90,119,131,74,185,153,99,49,193,43,39,192,191,240,70,143,141,63,7,62,37,127,193,55,63,97,191,2,252,58,248,179,240,211,199,222,54,248,55,251,16,126,199,62,19,248,189,224,255,0,5,120,239,194,222,42,241,79,194,175,21,39,192,111,10,104,239,225,159,137,30,30,208,181,89,238,252,15,226,1,171,120,99,196,182,166,207,83,134,214,224,92,248,118,250,3,24,150,210,225,35,77,165,187,176,30,153,255,0,14,157,255,0,130,89,127,210,52,255,0,96,15,252,67,127,217,215,255,0,157,205,31,240,233,223,248,37,151,253,35,79,246,0,255,0,196,55,253,157,127,249,220,215,223,217,250,255,0,158,188,117,165,166,7,230,127,196,79,248,35,127,252,18,227,226,7,195,255,0,28,248,14,219,246,6,253,142,254,28,220,120,219,193,222,38,240,141,191,196,47,133,191,178,207,236,241,225,15,137,190,4,159,196,154,45,238,141,15,140,254,29,120,178,63,133,243,55,133,252,119,165,201,122,183,218,70,162,176,202,108,181,11,27,123,145,20,134,45,135,248,209,253,135,254,44,73,240,187,76,213,108,254,45,69,101,240,223,86,248,102,218,181,143,196,237,51,198,118,55,254,14,185,248,127,169,248,97,238,173,252,103,166,120,202,63,19,220,219,175,132,239,244,139,251,93,118,29,74,29,65,237,228,211,230,210,101,134,240,67,52,50,37,127,161,239,139,124,89,225,95,1,120,87,196,222,57,241,215,137,124,63,224,191,4,120,47,195,250,207,139,60,99,227,31,22,107,58,119,135,60,43,225,63,10,248,115,78,184,214,60,67,226,95,19,120,135,88,185,134,211,66,240,253,134,145,103,121,117,123,123,117,52,86,214,182,214,146,207,60,137,18,51,15,227,191,254,10,69,255,0,4,171,253,160,62,59,252,74,241,103,252,20,199,246,73,248,1,167,248,47,192,151,177,222,252,78,241,31,236,137,121,167,120,175,194,255,0,180,71,199,15,20,120,95,89,208,181,173,31,246,178,185,248,49,123,161,69,107,161,252,90,241,29,188,154,174,175,31,194,221,74,77,47,197,26,106,248,11,68,241,150,179,167,205,241,211,197,122,183,128,124,15,195,142,195,44,77,59,107,120,244,238,175,249,232,84,101,107,249,159,111,126,206,223,16,46,126,60,107,62,19,248,143,241,98,194,255,0,68,248,55,224,141,111,68,241,175,193,79,129,190,33,211,231,209,245,31,19,248,199,195,119,176,107,158,20,253,160,126,61,248,102,250,36,184,139,83,210,245,216,116,205,79,225,215,129,175,226,117,240,165,220,54,126,58,241,117,187,252,74,131,194,250,63,193,223,119,253,164,62,56,233,250,142,157,168,72,215,8,25,34,185,45,27,27,121,119,124,135,203,103,203,171,161,54,242,134,86,121,20,188,151,8,187,78,8,31,204,55,131,63,224,160,118,223,15,111,252,73,225,79,30,93,93,124,63,241,47,129,99,241,29,143,142,252,55,227,63,63,195,250,247,130,181,79,11,234,139,166,107,218,111,140,180,125,114,56,238,188,53,171,105,207,101,36,26,141,189,253,173,181,197,140,214,114,195,120,177,205,28,138,105,248,203,246,212,241,119,197,239,21,63,195,79,131,190,24,241,159,197,255,0,138,90,162,220,191,134,254,27,252,49,208,181,159,25,248,247,94,109,55,195,55,30,33,191,93,7,193,222,24,130,125,71,87,107,123,29,46,91,219,145,5,162,249,22,122,125,197,221,196,145,218,69,60,203,242,83,192,86,169,90,41,194,212,224,218,140,82,127,213,251,190,166,252,246,141,155,249,157,15,197,63,11,71,251,91,254,212,255,0,179,255,0,236,211,29,166,191,170,233,31,28,254,54,252,57,240,31,140,161,248,123,99,61,215,141,180,191,134,158,36,241,77,148,31,20,188,87,225,166,150,195,80,183,134,93,11,225,187,248,219,91,158,254,123,43,237,63,75,180,240,204,186,158,163,11,233,214,183,27,138,254,138,191,224,143,95,240,72,255,0,31,126,203,190,53,241,95,237,97,251,101,105,191,11,252,67,251,71,248,131,69,209,52,159,130,250,6,134,247,158,45,213,255,0,103,63,12,107,190,30,154,111,137,67,82,241,116,178,166,147,63,197,125,106,235,88,143,66,212,219,67,183,212,108,244,109,47,193,215,22,90,7,139,117,77,39,197,58,196,12,87,214,224,176,138,133,21,25,171,201,235,175,79,35,9,180,222,157,15,209,47,248,40,215,252,20,107,224,143,252,19,119,224,139,124,77,248,154,100,241,103,196,15,22,73,168,232,63,3,190,7,104,90,141,189,151,141,126,49,248,218,206,222,222,89,108,44,38,150,222,127,248,70,252,19,166,253,187,78,159,196,126,35,158,218,123,77,18,210,246,4,75,125,71,90,212,116,77,19,87,254,69,63,104,207,248,40,39,252,20,19,254,10,71,253,169,225,127,17,120,206,127,217,251,224,15,137,78,171,96,62,1,252,16,189,212,244,72,124,65,225,61,96,120,207,70,135,71,248,185,241,5,101,139,93,248,167,246,143,9,248,181,52,159,16,216,201,117,164,120,35,93,143,74,182,189,255,0,132,34,206,242,7,106,242,79,248,40,239,199,127,8,126,220,127,240,84,191,140,127,20,190,30,252,65,240,223,199,15,130,94,2,210,126,28,124,26,248,25,227,127,12,46,136,254,20,255,0,132,67,193,222,27,177,214,124,115,103,225,93,118,202,194,221,188,109,160,75,241,199,196,223,22,46,45,181,215,151,81,183,213,45,111,161,184,209,53,43,157,3,251,34,90,253,16,253,149,254,14,233,87,130,217,164,211,2,130,177,25,93,158,209,195,23,10,22,54,69,117,1,74,44,152,224,170,136,130,128,6,43,131,50,204,37,69,206,156,29,148,116,249,255,0,86,42,17,190,189,94,199,231,79,195,111,248,39,7,135,99,186,26,141,191,133,237,33,146,235,72,145,93,226,178,134,221,124,195,115,104,103,137,100,134,8,73,2,226,38,4,9,29,78,206,141,128,227,103,226,31,252,19,127,195,215,145,233,179,222,120,106,218,88,172,211,81,147,115,217,197,114,176,166,203,70,144,254,246,43,157,131,100,57,254,2,112,126,247,27,127,170,239,134,63,0,116,189,66,43,81,29,177,142,77,241,177,242,144,230,88,88,199,36,177,188,130,50,10,179,166,246,80,225,87,39,169,0,87,67,241,35,246,123,211,116,248,110,124,203,111,49,138,170,197,27,6,49,70,172,145,137,146,41,66,183,148,36,49,140,176,126,121,224,128,13,124,203,205,106,42,171,223,109,175,95,63,242,55,228,125,207,228,255,0,224,47,237,35,251,124,255,0,193,53,221,108,63,103,127,138,18,234,191,11,45,35,186,149,63,103,79,139,182,122,135,143,126,7,35,92,175,140,181,103,62,26,240,223,246,197,174,163,240,234,47,237,239,26,95,234,183,95,240,134,107,94,26,58,198,175,28,23,94,32,143,89,183,180,123,87,254,177,127,224,152,31,240,86,207,131,95,240,82,125,3,197,218,13,183,134,228,248,33,251,71,252,55,146,242,247,226,7,236,245,226,15,20,219,248,163,84,139,193,141,171,45,134,135,241,43,192,30,42,254,195,210,127,225,96,248,18,97,119,165,90,234,119,17,233,150,87,122,14,179,120,186,118,169,101,21,173,247,135,245,77,119,241,183,246,165,248,53,164,91,219,222,202,186,104,117,103,159,47,28,150,176,108,147,113,148,28,9,73,14,199,205,36,142,67,41,25,201,231,240,255,0,193,31,17,236,255,0,99,95,219,191,246,93,253,167,238,53,235,127,134,94,25,248,87,241,195,193,247,159,20,188,126,218,62,159,226,20,208,254,6,248,142,244,120,47,227,219,93,104,177,233,26,132,247,182,146,252,30,241,7,142,81,254,193,103,54,169,11,79,231,232,230,45,66,27,54,143,232,178,236,198,85,28,97,55,204,165,167,153,140,225,187,234,143,244,181,164,35,63,150,63,206,43,224,31,248,123,23,252,18,203,254,146,89,251,0,247,255,0,155,200,253,157,187,127,221,70,163,254,30,197,255,0,4,178,255,0,164,150,126,192,31,248,153,31,179,175,255,0,60,106,250,19,35,137,255,0,130,203,124,52,248,115,241,35,254,9,101,251,127,255,0,194,196,240,7,130,188,125,255,0,8,7,236,109,251,85,124,74,240,39,252,38,190,21,208,188,85,255,0,8,87,196,111,10,254,206,191,20,63,225,24,241,255,0,132,191,183,108,39,255,0,132,115,198,218,111,219,175,190,193,170,217,249,55,246,127,109,151,236,243,199,230,62,239,191,254,29,124,52,248,113,240,127,193,186,55,195,175,132,191,15,252,19,240,187,225,247,135,127,180,63,225,31,240,39,195,175,10,232,94,9,240,110,133,253,173,170,223,107,154,175,246,55,134,60,51,97,107,99,166,125,167,91,212,245,43,203,143,34,4,243,174,181,9,238,36,221,52,178,59,126,59,255,0,193,66,127,224,161,95,176,47,199,191,216,23,246,225,248,23,240,47,246,225,253,144,62,52,124,109,248,209,251,31,254,210,223,9,254,14,252,29,248,77,251,75,124,24,248,141,241,79,226,199,197,63,136,191,6,60,107,224,255,0,135,223,13,62,26,124,62,240,127,141,111,53,111,29,252,64,215,124,91,172,233,26,94,141,163,105,118,151,90,142,169,168,234,182,214,54,54,211,220,207,20,79,251,125,74,202,247,182,172,2,138,40,166,7,249,92,127,193,54,177,255,0,10,215,225,151,31,243,40,120,95,255,0,76,214,35,61,58,226,191,172,207,217,80,159,236,235,47,250,236,152,246,196,54,248,252,122,126,94,184,35,249,51,255,0,130,109,127,201,53,248,101,255,0,98,135,133,255,0,244,205,97,95,214,95,236,169,255,0,32,219,47,250,236,191,250,38,218,190,47,55,111,218,85,245,255,0,35,166,159,217,242,71,238,39,193,127,249,117,247,242,243,199,95,187,222,185,15,141,93,47,58,159,146,78,167,39,167,114,122,159,173,117,255,0,5,255,0,229,215,254,217,127,236,181,200,124,106,233,121,254,228,159,202,190,86,63,197,127,215,115,115,240,139,246,178,226,9,56,25,31,107,29,251,152,9,3,158,7,94,58,115,197,127,35,255,0,240,82,60,127,194,17,227,159,109,23,88,199,3,131,246,73,143,25,28,115,95,215,7,237,103,254,166,95,251,123,254,112,87,242,63,255,0,5,35,255,0,145,35,199,63,246,5,214,127,244,142,106,250,236,165,47,109,13,55,103,60,254,215,204,255,0,90,74,40,162,190,192,231,10,40,162,128,10,40,162,128,63,202,227,254,9,181,255,0,36,215,225,151,253,138,30,23,255,0,211,53,133,127,89,127,178,167,252,131,108,191,235,178,255,0,232,155,106,254,77,63,224,155,95,242,77,126,25,127,216,161,225,127,253,51,88,87,245,151,251,42,127,200,54,203,254,187,47,254,137,182,175,139,205,255,0,139,83,215,255,0,145,58,105,239,31,79,208,253,196,248,47,255,0,46,191,246,203,255,0,101,174,67,227,87,75,207,247,36,254,85,215,252,23,255,0,151,95,251,101,255,0,178,215,33,241,171,165,231,251,146,127,42,249,88,255,0,21,255,0,93,205,207,194,47,218,207,253,76,191,246,247,252,224,175,228,127,254,10,71,255,0,34,71,142,127,236,11,172,255,0,233,28,213,253,112,126,214,127,234,101,255,0,183,191,231,5,127,35,255,0,240,82,63,249,18,60,115,255,0,96,93,103,255,0,72,230,175,175,202,127,141,79,252,71,60,254,215,204,255,0,90,74,40,162,190,188,231,10,40,162,128,10,40,162,128,63,202,227,254,9,181,255,0,36,215,225,151,253,138,30,23,255,0,211,53,133,127,89,127,178,167,252,131,108,191,235,178,255,0,232,155,106,254,77,63,224,155,95,242,77,126,25,127,216,161,225,127,253,51,88,87,245,151,251,42,127,200,54,203,254,187,47,254,137,182,175,139,205,255,0,139,83,215,255,0,145,58,105,239,31,79,208,253,196,248,47,255,0,46,191,246,203,255,0,101,174,67,227,87,75,207,247,36,254,85,215,252,23,255,0,151,95,251,101,255,0,178,215,33,241,171,165,231,251,146,127,42,249,88,255,0,21,255,0,93,205,207,194,47,218,207,253,76,191,246,247,252,224,175,228,127,254,10,71,255,0,34,71,142,127,236,11,172,255,0,233,28,213,253,112,126,214,127,234,101,255,0,183,191,231,5,127,35,255,0,240,82,63,249,18,60,115,255,0,96,93,103,255,0,72,230,175,175,202,127,141,79,252,71,60,254,215,204,255,0,90,74,40,162,190,188,231,10,40,162,128,10,40,162,128,63,202,227,254,9,181,255,0,36,215,225,151,253,138,30,23,255,0,211,53,133,127,89,127,178,167,252,131,108,191,235,178,255,0,232,155,106,254,77,63,224,155,95,242,77,126,25,127,216,161,225,127,253,51,88,87,245,151,251,42,127,200,54,203,254,187,47,254,137,182,175,139,205,255,0,139,83,215,255,0,145,58,105,239,31,79,208,253,196,248,47,255,0,46,191,246,203,255,0,101,174,67,227,87,75,207,247,36,254,85,215,252,23,255,0,151,95,251,101,255,0,178,215,33,241,171,165,231,251,146,127,42,249,88,255,0,21,255,0,93,205,207,194,47,218,207,253,76,191,246,247,252,224,175,228,127,254,10,71,255,0,34,71,142,127,236,11,172,255,0,233,28,213,253,112,126,214,127,234,101,255,0,183,191,231,5,127,35,255,0,240,82,63,249,18,60,115,255,0,96,93,103,255,0,72,230,175,175,202,127,141,79,252,71,60,254,215,204,255,0,90,74,40,162,190,188,231,10,40,162,128,10,40,162,128,63,202,227,254,9,181,255,0,36,215,225,151,253,138,30,23,255,0,211,53,133,127,89,127,178,167,252,131,108,191,235,178,255,0,232,155,106,254,77,63,224,155,95,242,77,126,25,127,216,161,225,127,253,51,88,87,245,151,251,42,127,200,54,203,254,187,47,254,137,182,175,139,205,255,0,139,83,215,255,0,145,58,105,239,31,79,208,253,196,248,47,255,0,46,191,246,203,255,0,101,174,67,227,87,75,207,247,36,254,85,215,252,23,255,0,151,95,251,101,255,0,178,215,33,241,171,165,231,251,146,127,42,249,88,255,0,21,255,0,93,205,207,194,47,218,207,253,76,191,246,247,252,224,175,228,127,254,10,71,255,0,34,71,142,127,236,11,172,255,0,233,28,213,253,112,126,214,127,234,101,255,0,183,191,231,5,127,35,255,0,240,82,63,249,18,60,115,255,0,96,93,103,255,0,72,230,175,175,202,127,141,79,252,71,60,254,215,204,255,0,90,74,40,162,190,188,231,10,40,162,128,10,40,162,128,63,202,227,254,9,181,255,0,36,215,225,151,253,138,30,23,255,0,211,53,133,127,89,127,178,167,252,131,108,191,235,178,255,0,232,155,106,254,77,63,224,155,95,242,77,126,25,127,216,161,225,127,253,51,88,87,245,151,251,42,127,200,54,203,254,187,47,254,137,182,175,139,205,255,0,139,83,215,255,0,145,58,105,239,31,79,208,253,196,248,47,255,0,46,191,246,203,255,0,101,174,67,227,87,75,207,247,36,254,85,215,252,23,255,0,151,95,251,101,255,0,178,215,33,241,171,165,231,251,146,127,42,249,88,255,0,21,255,0,93,205,207,194,47,218,207,253,76,191,246,247,252,224,175,228,127,254,10,71,255,0,34,71,142,127,236,11,172,255,0,233,28,213,253,112,126,214,127,234,101,255,0,183,191,231,5,127,35,255,0,240,82,63,249,18,60,115,255,0,96,93,103,255,0,72,230,175,175,202,127,141,79,252,71,60,254,215,204,255,0,90,74,40,162,190,188,231,10,40,162,128,10,40,162,128,63,202,227,254,9,181,255,0,36,215,225,159,253,138,30,23,255,0,211,61,141,127,89,127,178,167,252,131,108,191,235,178,255,0,232,155,106,254,96,126,1,124,37,241,135,236,117,241,59,196,255,0,178,143,198,123,88,244,207,138,127,0,53,230,248,97,226,209,105,14,169,103,163,107,87,126,26,138,222,210,195,198,30,18,30,42,211,116,205,71,80,240,22,187,163,141,59,91,240,246,161,117,167,89,54,169,161,235,250,126,164,150,241,197,116,130,191,165,175,217,71,197,122,60,150,150,176,36,255,0,58,180,82,115,37,174,10,58,197,24,101,31,105,201,195,71,207,167,152,188,243,199,198,102,233,251,74,158,191,229,254,71,68,45,120,179,247,163,224,191,252,186,255,0,219,47,253,150,185,15,141,93,47,63,220,147,249,86,151,193,159,17,233,105,246,109,242,186,143,221,124,197,1,65,194,96,111,86,193,57,32,112,79,39,29,120,170,31,25,4,87,17,220,188,87,86,172,174,172,3,7,118,140,49,0,144,100,72,138,144,1,25,57,199,61,113,205,124,170,77,85,119,235,255,0,4,232,63,7,255,0,107,63,245,50,255,0,219,223,243,130,191,145,255,0,248,41,31,252,137,30,57,255,0,176,46,179,255,0,164,115,87,245,229,251,88,233,193,173,167,113,168,105,163,105,184,27,90,232,7,109,225,8,40,54,29,202,60,188,19,198,12,138,49,205,127,41,223,183,87,195,47,21,124,74,138,247,225,239,129,109,151,197,62,54,241,221,220,126,13,240,119,133,116,8,117,45,111,196,62,38,241,87,137,174,19,68,240,247,135,116,29,15,72,211,231,187,214,117,187,221,94,246,210,218,214,210,214,25,174,110,46,46,163,134,24,164,149,213,79,213,229,82,181,106,111,165,215,230,255,0,200,231,159,218,249,159,234,199,69,124,1,255,0,13,145,251,69,127,210,39,191,111,255,0,252,56,223,240,75,47,254,153,101,31,240,217,31,180,87,253,34,119,246,255,0,255,0,195,141,255,0,4,178,255,0,233,150,87,217,28,231,223,244,87,231,7,139,63,224,160,62,62,248,99,225,95,18,252,74,248,213,255,0,4,230,253,183,254,13,124,28,248,121,225,253,103,199,63,22,190,47,248,179,197,95,240,79,79,19,248,87,225,79,195,63,9,233,215,58,255,0,143,62,36,248,155,195,127,9,63,111,111,18,248,175,196,62,31,208,188,45,167,234,186,165,237,143,134,60,57,226,15,16,221,219,105,114,91,232,186,38,171,169,73,109,99,113,250,63,64,5,20,81,64,31,199,95,252,23,183,254,9,253,226,207,131,127,22,252,115,255,0,5,82,240,30,188,158,41,248,107,227,27,143,133,122,31,237,43,224,207,16,93,233,122,126,167,240,191,90,211,180,207,5,124,19,248,127,241,3,192,243,42,219,31,18,248,23,84,26,127,129,244,173,87,76,115,113,172,105,58,205,212,122,173,187,106,58,30,167,168,47,132,190,52,253,152,127,105,253,62,222,214,194,246,223,81,179,120,156,64,23,100,176,195,54,103,194,192,147,149,3,97,121,35,101,221,157,167,201,46,193,112,86,191,188,79,18,248,107,195,158,52,240,231,136,60,29,227,31,15,232,158,44,240,143,139,52,77,87,195,94,41,240,183,137,116,171,13,119,195,158,37,240,230,187,97,62,151,174,120,127,196,26,30,169,4,182,186,206,137,121,166,93,93,91,93,218,92,197,36,23,16,92,201,12,209,188,110,202,127,158,63,219,27,254,13,221,248,31,241,6,109,123,226,63,236,63,227,57,255,0,100,239,138,87,7,91,214,87,225,204,209,95,120,151,246,112,241,86,181,123,23,140,181,102,211,224,208,22,99,172,124,19,75,239,16,107,94,28,177,73,188,55,117,121,225,111,13,232,122,19,195,164,124,58,185,185,153,157,188,188,118,94,177,41,202,14,210,123,249,151,25,219,71,177,139,240,235,246,182,211,60,136,54,234,162,3,229,27,225,181,227,65,32,138,88,163,139,127,155,128,99,146,35,188,180,114,43,48,151,118,7,109,191,30,126,215,26,124,150,178,31,237,81,112,215,17,76,230,3,36,115,41,123,72,225,49,196,161,29,152,238,222,195,15,32,12,95,229,246,252,196,241,119,252,17,223,254,10,255,0,240,131,74,183,241,23,135,244,127,128,31,30,238,110,239,97,208,155,193,159,5,254,51,205,166,120,155,75,180,184,183,184,190,111,17,223,77,241,215,193,63,15,244,134,240,244,18,105,144,218,200,150,218,205,206,163,246,141,98,208,195,166,79,108,183,151,86,126,97,251,57,255,0,193,57,255,0,224,169,223,182,175,194,207,3,124,107,240,110,129,240,131,225,7,195,63,30,248,3,192,95,20,190,23,120,155,227,127,197,184,237,255,0,225,104,248,23,226,183,135,109,252,89,225,141,119,194,250,55,194,45,3,199,90,134,133,229,104,13,164,92,94,219,120,146,211,195,183,176,255,0,194,71,105,20,86,247,83,199,168,71,167,248,79,35,169,207,172,109,126,191,240,77,125,175,247,191,15,248,7,77,251,75,126,212,26,117,253,182,163,50,234,150,62,68,69,192,146,121,146,64,166,100,243,194,160,35,247,146,152,23,121,10,118,110,141,129,113,183,45,199,255,0,193,30,63,99,61,99,254,10,27,251,92,105,191,180,166,165,172,235,30,9,248,9,251,21,124,93,248,107,227,230,214,244,221,26,247,237,255,0,23,190,54,248,87,89,139,199,126,13,248,107,225,175,16,234,26,52,186,52,122,38,147,123,160,232,90,175,141,94,41,164,213,173,180,253,119,68,210,237,44,109,143,137,211,94,209,63,81,63,101,95,248,55,27,195,178,220,197,227,127,248,40,111,197,217,190,51,235,18,125,173,87,224,55,193,61,107,197,94,12,248,63,167,196,243,248,202,193,98,241,15,197,67,6,147,227,15,28,43,216,234,62,17,213,44,134,139,7,128,87,75,212,52,203,189,46,253,124,79,164,204,90,95,233,67,225,215,195,79,135,63,7,252,27,163,252,58,248,73,224,15,4,252,46,248,125,225,239,237,15,248,71,252,9,240,235,194,154,23,130,124,27,161,127,107,106,151,218,230,171,253,143,225,143,12,216,90,216,233,159,106,214,245,61,74,242,227,201,129,60,235,173,66,123,137,55,77,52,142,222,214,7,45,88,102,167,82,92,207,162,236,103,41,183,123,117,59,106,43,159,241,102,179,168,248,115,194,190,38,241,14,143,225,47,16,120,247,86,208,124,63,172,235,58,87,129,124,39,115,225,91,63,20,248,211,81,210,244,219,155,235,47,9,248,102,239,199,94,38,209,116,75,77,127,81,185,130,43,59,41,53,157,103,73,210,146,230,242,54,212,53,59,11,65,53,212,92,39,132,190,60,124,32,241,167,194,23,248,245,163,248,247,68,179,248,73,101,162,120,163,95,241,15,140,188,88,215,94,2,180,240,45,167,128,174,181,141,59,226,69,135,196,237,59,199,86,218,117,239,194,191,18,248,87,90,240,231,137,116,223,22,232,254,35,182,210,245,111,10,106,222,23,213,116,143,17,89,105,154,158,153,127,105,111,235,182,150,173,217,25,159,45,255,0,193,88,191,229,22,95,240,82,207,251,48,15,219,35,255,0,89,215,226,53,125,255,0,95,205,231,252,21,31,254,11,17,251,12,248,251,246,41,253,181,127,102,159,130,62,61,241,87,237,5,241,139,226,247,192,47,218,63,246,115,240,254,131,240,155,192,94,35,185,240,245,143,137,188,127,240,155,226,23,128,116,159,26,93,252,71,241,173,182,137,225,157,127,225,148,126,41,188,210,97,125,83,195,250,198,181,37,212,26,213,181,254,149,105,168,233,230,107,168,126,206,248,47,255,0,5,202,255,0,130,112,124,95,127,14,105,90,199,198,155,207,128,190,50,215,211,91,146,235,193,255,0,180,79,132,245,175,134,113,120,73,52,88,245,107,197,127,25,124,81,104,175,60,3,161,173,238,143,165,37,221,129,62,46,144,92,127,107,217,105,231,102,179,63,246,98,71,181,166,223,47,58,191,168,218,107,117,99,245,214,138,40,171,16,81,69,20,0,87,192,31,240,73,223,249,69,151,252,19,76,231,254,108,3,246,55,255,0,214,117,248,115,249,215,223,245,249,3,251,39,120,75,254,10,155,251,47,254,203,31,179,79,236,209,255,0,12,191,251,0,120,227,254,25,223,224,7,193,191,129,127,240,154,255,0,195,196,127,104,175,12,255,0,194,97,255,0,10,147,225,215,135,60,1,255,0,9,79,252,35,127,240,235,77,67,254,17,255,0,237,15,248,71,254,215,246,31,183,223,125,147,237,126,71,219,46,124,191,57,192,63,95,168,175,128,63,225,99,127,193,83,127,232,205,255,0,96,15,252,89,103,237,21,255,0,210,157,163,254,22,55,252,21,55,254,140,223,246,0,255,0,197,150,126,209,95,253,41,218,0,251,244,245,62,152,201,252,63,175,249,226,191,207,199,254,10,7,251,81,107,159,240,80,207,218,163,226,87,196,31,217,14,111,138,250,47,236,171,170,104,254,13,240,223,142,190,17,89,120,199,84,248,119,224,143,219,99,196,94,0,212,65,183,253,162,188,75,240,230,242,239,72,210,108,254,47,127,194,63,161,124,56,210,252,41,119,226,227,61,253,239,134,190,13,248,55,76,241,29,247,133,92,88,71,224,47,234,171,246,143,248,187,255,0,5,120,240,255,0,236,241,241,231,94,240,31,236,145,251,29,232,254,57,209,62,12,124,81,213,252,25,171,252,45,253,184,126,57,252,104,248,155,165,120,175,77,240,62,187,123,225,221,75,225,215,193,223,16,127,193,45,116,251,15,139,30,59,131,88,134,206,93,35,195,55,215,246,54,122,246,161,21,190,151,115,121,109,13,211,204,159,202,23,252,19,127,75,240,68,26,31,135,23,79,151,77,104,82,222,216,66,209,107,18,78,165,17,118,219,237,118,189,111,49,76,81,88,16,121,220,54,158,119,146,222,110,101,94,84,40,169,37,117,253,127,95,137,112,87,126,159,240,79,173,255,0,102,95,217,23,192,63,17,188,25,164,120,191,193,150,246,250,246,133,169,90,106,182,22,210,182,153,123,167,222,89,234,58,86,172,250,102,191,225,237,103,195,186,181,189,181,231,133,252,83,164,235,218,110,179,165,234,154,70,161,5,166,171,164,106,26,29,238,155,169,216,218,222,218,92,195,20,63,31,63,97,29,17,108,181,80,250,76,118,169,34,150,118,88,200,88,218,11,24,160,85,17,9,64,132,169,121,91,122,57,41,45,184,37,163,96,69,126,192,120,35,224,231,138,60,47,171,94,124,106,253,152,97,248,125,123,241,7,196,159,217,246,127,20,254,13,252,83,241,111,136,62,31,252,27,248,233,5,134,153,109,225,237,7,198,26,151,143,188,37,240,243,197,87,255,0,11,126,51,248,119,65,181,210,163,180,241,85,143,134,188,68,60,79,225,173,21,60,21,226,157,34,253,108,188,5,226,143,134,186,158,61,248,151,240,195,227,15,131,53,109,115,65,178,214,124,45,226,15,13,234,186,167,131,190,34,124,52,241,173,174,159,163,248,255,0,225,55,196,77,55,78,177,213,117,191,135,191,16,52,237,35,81,189,181,179,215,32,179,214,116,59,251,43,189,55,84,212,244,15,16,232,158,37,210,188,79,225,77,103,196,30,18,215,116,61,111,82,249,9,87,169,206,171,82,110,80,190,170,247,148,91,233,37,218,255,0,106,214,126,78,233,116,56,232,239,164,150,183,232,253,15,204,143,248,36,87,252,20,31,197,223,178,103,199,171,15,216,191,246,152,241,159,197,143,27,124,28,248,229,226,79,134,223,15,127,102,159,16,120,139,87,190,241,214,151,240,43,226,59,152,126,30,248,87,225,109,141,188,218,125,206,169,167,124,44,241,26,201,224,157,30,194,59,59,239,236,63,10,106,90,38,159,60,58,13,150,155,226,31,19,107,218,81,95,151,191,182,93,239,136,188,41,241,39,192,158,44,248,22,151,119,95,29,188,43,241,63,193,30,36,248,31,103,225,173,61,60,101,226,91,159,139,218,15,139,52,221,87,225,133,191,134,252,21,61,181,244,94,48,215,159,199,58,127,133,150,203,75,146,198,250,61,70,230,88,44,222,206,232,93,24,37,43,235,240,24,151,83,15,23,52,219,90,105,169,205,36,147,86,210,232,255,0,69,122,40,162,189,34,66,138,40,160,2,138,40,160,2,191,207,255,0,246,142,253,154,53,191,248,36,39,237,71,167,124,15,212,60,85,170,120,183,224,127,142,52,73,60,119,251,63,124,70,214,52,59,221,50,255,0,82,240,133,134,173,54,149,171,120,15,197,218,245,190,143,103,165,234,95,20,252,52,98,210,34,213,255,0,177,230,150,218,123,45,111,195,222,33,184,180,208,87,196,137,161,233,159,232,1,94,107,241,139,224,247,195,63,218,3,225,143,141,62,13,124,99,240,126,151,227,223,134,159,16,116,105,116,47,21,248,91,87,251,84,118,247,246,111,44,55,86,183,86,119,250,125,196,55,122,30,187,103,169,91,89,95,105,154,157,133,197,174,165,165,106,58,117,174,165,166,221,218,95,218,219,220,69,207,137,195,199,19,77,194,90,62,143,177,81,147,139,208,254,91,62,10,254,218,250,84,118,81,71,6,179,10,191,217,101,156,35,220,70,28,24,154,27,86,105,224,121,149,73,18,201,116,1,44,65,14,24,33,56,97,243,55,237,119,251,69,88,222,107,179,124,92,248,127,226,61,43,195,223,23,52,29,10,63,13,106,18,94,95,207,105,225,191,138,94,4,180,123,239,16,219,124,49,248,159,253,159,28,210,67,162,218,223,235,62,38,191,240,222,191,12,87,122,167,130,117,111,16,95,93,233,145,94,232,154,231,139,252,39,226,207,211,239,138,191,240,110,47,193,11,211,127,168,126,205,223,180,207,198,239,129,218,166,161,226,123,187,232,244,79,28,216,248,115,227,159,195,127,14,120,50,245,245,43,175,248,66,188,39,225,233,63,225,26,215,109,228,180,187,147,68,142,199,84,213,188,83,173,93,173,150,149,52,55,235,168,222,222,29,74,14,143,225,31,252,27,159,251,45,104,191,240,143,106,223,180,103,198,63,142,127,180,110,187,106,53,145,226,255,0,14,91,235,150,159,6,126,15,248,187,237,81,234,182,58,3,65,225,111,3,164,190,44,240,188,154,118,159,115,163,79,190,207,199,133,174,245,93,20,221,74,87,78,186,151,71,30,37,60,154,164,43,41,197,165,21,167,147,93,154,255,0,128,104,234,108,214,182,252,15,199,143,248,36,111,236,213,175,255,0,193,77,63,106,109,31,246,150,213,245,111,19,252,60,248,29,251,24,252,88,248,101,227,157,98,210,127,14,75,7,138,60,105,241,255,0,194,58,221,159,196,63,11,124,34,211,60,67,61,133,238,135,253,159,163,223,120,127,195,90,167,139,238,45,117,43,251,161,161,235,94,31,135,72,134,75,63,26,88,120,171,71,43,224,143,248,43,247,252,28,65,225,253,51,198,127,182,223,236,51,240,59,246,24,209,254,5,120,199,225,190,141,251,73,127,193,49,245,31,140,30,24,248,235,101,47,195,223,136,255,0,179,79,133,245,111,137,159,1,135,132,124,115,251,59,233,63,1,116,171,107,157,27,68,211,181,127,18,248,143,225,172,118,158,37,180,212,190,27,120,143,88,186,135,73,214,47,124,29,226,79,136,222,16,241,241,94,245,26,48,161,5,78,10,200,201,183,39,118,127,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 8698; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

