#ifndef GETVESUITE_RGibbs_RGibbs_ICON1_H
#define GETVESUITE_RGibbs_RGibbs_ICON1_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_RGibbs_RGibbs_ICON1( void )
{
    unsigned char osgData[ 6364 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,125,0,63,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,254,43,243,251,246,235,255,0,130,154,126,201,191,240,79,79,10,166,173,241,207,198,205,171,120,243,80,95,15,220,248,107,224,47,195,139,175,11,248,135,227,191,138,244,127,16,234,186,174,153,23,138,180,175,2,107,62,39,211,23,76,240,53,185,240,239,137,222,227,94,213,239,52,205,27,206,240,244,186,93,189,253,198,187,115,167,105,87,177,127,193,78,191,111,15,10,127,193,59,191,100,143,28,124,119,213,160,109,83,199,186,171,92,124,55,248,7,225,103,240,238,169,226,61,51,197,255,0,30,124,71,225,159,19,106,222,1,210,60,81,6,155,171,233,163,78,240,61,186,248,111,87,213,181,251,169,117,93,53,215,69,240,213,252,58,117,196,218,212,218,101,133,231,249,217,105,58,71,196,191,218,135,226,103,139,254,60,252,123,241,142,183,241,67,226,151,197,45,121,188,65,226,223,23,248,142,91,121,47,60,71,127,113,13,182,159,103,186,222,202,8,173,60,59,160,105,250,93,189,189,142,147,164,217,71,14,155,163,105,182,118,58,118,157,99,109,105,111,4,3,159,17,136,141,8,221,234,216,210,109,216,253,8,253,162,191,224,181,223,240,84,15,219,47,85,241,62,159,224,111,137,47,251,43,124,31,214,174,108,180,253,55,192,63,0,177,225,191,20,89,104,186,127,141,174,252,83,161,106,154,159,199,203,203,15,248,76,174,188,95,253,147,39,135,116,237,106,251,195,151,254,17,209,181,75,93,33,162,93,2,206,223,82,212,160,185,252,253,241,239,195,175,139,223,31,117,171,79,22,252,124,248,195,241,23,227,119,141,180,237,34,31,15,233,222,37,248,171,227,79,22,252,87,241,62,153,225,123,43,219,237,70,203,64,180,215,188,117,168,234,119,118,122,36,90,174,177,172,92,199,105,12,226,214,57,181,139,137,150,53,158,230,102,127,209,95,134,127,179,221,186,219,65,29,245,144,43,44,74,139,99,12,66,72,161,102,32,31,44,7,207,12,246,241,239,126,20,68,232,54,141,162,190,188,209,191,103,248,210,225,164,77,38,214,21,123,118,196,178,252,241,186,179,196,192,46,85,1,200,25,7,113,225,79,29,199,129,91,50,155,110,243,178,242,232,106,161,30,186,159,136,63,15,52,31,218,19,246,121,58,218,254,206,191,180,7,197,239,129,178,120,172,233,139,227,56,126,16,252,78,241,255,0,194,41,188,77,253,136,183,239,225,215,241,26,248,7,93,210,134,190,182,43,172,235,98,211,237,134,99,107,253,181,112,109,132,98,230,224,191,234,23,236,135,255,0,5,244,253,191,63,101,63,25,248,27,195,95,181,94,181,47,237,85,251,63,65,15,131,188,57,226,104,124,75,160,248,118,219,227,183,133,60,31,225,221,47,84,209,46,53,159,2,124,77,209,163,210,23,199,62,60,152,223,248,126,255,0,81,255,0,132,252,235,55,30,34,147,195,113,219,31,17,232,23,122,173,254,186,61,183,95,253,159,32,148,234,13,54,143,4,168,240,20,121,226,192,67,186,216,70,2,41,70,5,249,10,6,254,72,198,65,60,124,95,241,91,246,122,183,187,183,187,75,123,36,154,210,68,242,164,178,157,124,152,150,39,64,146,63,204,75,67,39,146,241,62,8,40,205,3,62,209,181,90,157,28,198,73,171,77,191,87,232,39,5,211,67,251,166,253,143,127,110,15,217,179,246,233,248,99,167,124,77,253,158,62,34,232,254,38,31,216,250,38,165,227,95,135,151,154,142,145,109,241,91,225,30,161,174,205,172,217,218,248,107,226,207,129,108,245,75,155,159,7,107,45,168,248,111,196,112,218,74,237,46,153,172,197,161,205,169,120,127,81,213,244,121,45,181,27,143,173,107,252,178,62,30,124,82,248,237,255,0,4,247,248,243,225,159,218,99,246,104,241,41,240,207,141,188,52,103,211,245,29,39,81,73,110,124,29,227,207,9,106,55,22,119,26,231,194,175,136,158,28,75,219,113,226,143,135,218,164,214,22,237,36,75,119,29,214,159,117,13,134,179,164,94,105,186,190,159,167,234,86,223,233,103,251,41,254,211,159,11,127,108,175,217,243,225,151,237,47,240,98,231,93,159,225,215,197,61,30,247,82,209,237,252,83,162,79,225,223,19,104,218,150,135,174,106,190,20,241,103,133,252,69,164,77,36,137,111,174,233,30,48,208,117,237,46,241,237,46,47,52,233,238,52,121,46,52,203,253,67,78,150,214,250,227,223,161,94,53,227,120,238,183,51,106,205,174,199,240,117,255,0,5,176,253,162,47,191,108,223,248,41,239,196,79,4,88,248,142,227,91,248,77,251,40,92,191,192,111,135,246,54,54,126,50,177,209,52,255,0,20,120,122,45,62,227,227,206,171,125,160,120,170,224,218,197,227,57,126,48,55,137,252,53,168,107,154,117,133,165,166,173,162,124,42,240,252,112,155,251,123,40,181,43,205,15,217,243,225,157,178,195,99,127,28,81,202,165,82,11,40,163,64,198,36,152,29,137,202,22,71,4,23,111,41,16,42,185,0,144,138,163,242,115,194,127,181,79,195,79,139,255,0,24,126,47,252,123,248,149,241,83,224,223,132,188,113,241,171,226,63,142,62,42,248,155,75,181,241,198,129,160,248,95,78,241,55,197,47,22,234,222,56,241,37,150,135,101,175,120,138,230,242,215,67,182,214,124,73,169,71,107,29,197,229,221,196,54,214,241,37,197,197,196,168,242,201,251,27,240,63,246,179,253,145,244,104,244,164,214,63,106,239,217,146,192,91,91,230,117,191,248,235,240,202,209,72,109,153,143,23,94,36,92,182,34,32,169,3,239,14,185,175,15,48,117,231,57,56,194,82,79,109,31,150,218,23,11,90,253,127,225,143,212,127,133,191,11,99,146,56,191,116,5,214,17,110,46,118,98,85,147,12,81,81,9,102,35,115,135,59,242,65,101,69,80,193,86,190,203,240,223,193,181,184,68,154,29,61,88,155,98,60,201,17,183,48,18,71,180,188,106,65,18,21,40,78,88,147,212,142,120,248,243,225,79,237,217,255,0,4,253,180,75,24,181,111,219,139,246,50,177,27,85,231,107,207,218,111,224,197,162,7,7,228,105,4,254,50,80,100,86,243,79,204,58,178,238,207,24,251,183,194,159,240,80,255,0,248,38,67,196,159,110,255,0,130,135,126,194,214,138,27,17,193,115,251,92,254,207,214,203,183,203,40,94,77,255,0,16,70,24,177,76,101,120,0,0,0,83,143,155,173,75,25,123,170,19,127,246,236,159,111,47,248,39,68,121,44,157,245,245,56,95,21,124,29,88,18,229,154,200,65,43,236,85,154,37,102,112,198,36,216,203,25,5,130,239,242,195,16,91,175,35,7,143,135,126,42,124,50,72,210,228,197,108,145,222,36,76,75,42,185,19,67,134,118,82,80,6,0,43,148,202,41,66,28,50,175,201,242,254,132,120,199,254,10,25,255,0,4,204,49,201,21,151,252,20,43,246,26,189,134,97,42,21,131,246,180,253,159,238,86,38,124,236,43,179,199,238,124,172,21,28,166,56,201,1,148,99,224,15,138,223,183,71,236,11,118,151,41,165,254,219,127,177,198,160,177,72,76,107,101,251,74,252,27,189,15,3,18,76,112,8,252,98,251,191,229,168,35,184,145,65,36,96,7,66,158,38,234,248,121,173,183,140,151,203,97,75,149,234,164,174,126,57,254,209,63,11,172,158,27,201,94,24,82,219,80,89,99,154,25,17,81,218,121,26,72,162,145,163,216,201,190,50,225,92,52,43,193,227,123,32,53,246,151,252,27,89,251,75,106,63,4,63,108,223,137,255,0,177,111,136,188,73,115,107,240,251,246,139,240,150,191,227,111,1,120,123,84,179,241,165,234,73,241,235,225,101,157,182,177,49,240,181,165,133,220,154,55,131,167,214,254,11,67,241,10,235,196,87,247,182,112,190,176,126,14,120,110,210,61,74,43,139,59,123,13,67,229,143,143,191,181,63,236,167,173,217,107,54,250,63,237,85,251,53,106,114,72,242,73,23,216,62,56,124,55,188,89,85,227,216,4,127,101,241,19,151,57,140,231,3,163,15,90,252,194,240,79,237,247,160,254,198,95,181,47,195,191,218,215,224,47,143,62,5,120,247,226,47,195,33,226,139,173,3,69,241,143,138,108,188,81,224,139,187,143,26,248,23,197,31,13,117,120,181,141,59,193,158,50,210,175,110,22,47,15,120,235,91,158,221,96,212,160,49,221,217,219,201,41,150,8,230,183,155,233,242,247,90,18,135,53,57,69,61,53,77,105,167,146,252,76,38,147,87,190,191,153,243,191,236,186,63,209,63,100,3,158,158,56,240,249,245,255,0,155,98,248,178,56,207,31,165,126,192,147,255,0,24,223,255,0,5,124,255,0,177,75,226,201,252,127,225,221,159,4,57,255,0,26,252,127,253,151,127,227,207,246,65,255,0,177,223,195,255,0,250,204,95,22,107,246,0,255,0,201,184,127,193,95,63,236,82,248,179,255,0,174,236,248,33,79,23,241,124,227,255,0,167,98,11,120,255,0,93,17,251,219,227,127,249,46,159,240,77,62,127,230,241,60,103,255,0,174,249,253,189,61,107,123,226,183,252,153,151,252,28,165,237,164,254,209,24,250,127,195,147,191,99,62,61,187,126,93,235,7,198,255,0,242,93,127,224,154,127,246,120,158,52,255,0,215,124,126,222,149,189,241,87,254,76,203,254,14,83,255,0,176,79,237,17,255,0,174,78,253,140,235,231,222,208,215,172,127,245,37,27,218,219,105,123,254,72,250,247,246,137,255,0,148,145,254,202,95,246,100,31,240,81,15,253,95,31,240,77,17,254,126,181,248,77,241,151,254,81,115,255,0,5,3,255,0,177,175,254,11,44,61,255,0,228,234,63,108,35,215,191,255,0,90,191,118,127,104,159,249,73,31,236,165,255,0,102,65,255,0,5,16,255,0,213,243,255,0,4,209,175,194,111,140,191,242,139,143,248,40,31,253,141,159,240,89,111,253,106,143,219,10,181,165,255,0,46,189,105,127,233,202,128,246,254,188,143,144,255,0,107,94,127,105,143,134,95,246,64,127,106,46,220,255,0,201,65,253,144,205,127,53,159,181,63,252,146,191,136,223,246,89,254,43,253,115,255,0,13,21,241,15,255,0,175,95,210,159,237,107,255,0,39,49,240,207,254,200,15,237,69,255,0,171,7,246,67,175,230,179,246,167,255,0,146,85,241,27,254,203,63,197,127,253,104,175,136,117,244,24,31,177,233,31,253,42,71,60,247,254,187,35,234,15,217,119,254,60,255,0,100,31,251,29,252,63,255,0,172,197,241,102,191,96,15,252,155,135,252,21,243,254,197,47,139,63,250,238,207,130,21,248,255,0,251,46,255,0,199,159,236,131,255,0,99,191,135,255,0,245,152,190,44,215,236,1,255,0,147,112,255,0,130,190,127,216,165,241,103,255,0,93,217,240,66,178,197,252,95,56,255,0,233,216,141,111,31,235,162,63,123,124,111,255,0,37,215,254,9,167,255,0,103,137,227,79,253,119,199,237,233,91,223,21,127,228,204,191,224,229,63,251,4,254,209,31,250,228,239,216,206,176,124,111,255,0,37,215,254,9,167,255,0,103,137,227,79,253,119,199,237,233,91,223,21,127,228,204,191,224,229,63,251,4,254,209,31,250,228,239,216,206,190,125,237,15,85,255,0,169,49,55,237,232,255,0,244,148,125,123,251,68,255,0,202,72,255,0,101,47,251,50,15,248,40,135,254,175,159,248,38,141,126,19,124,101,255,0,148,92,127,193,64,255,0,236,108,255,0,130,203,127,235,84,126,216,85,251,179,251,68,255,0,202,72,255,0,101,47,251,50,15,248,40,135,254,175,159,248,38,141,126,19,124,101,255,0,148,92,127,193,64,255,0,236,108,255,0,130,203,127,235,84,126,216,85,173,31,249,117,235,75,255,0,78,84,7,183,203,255,0,145,62,67,253,173,127,228,230,62,25,255,0,217,1,253,168,191,245,96,254,200,117,252,214,126,212,255,0,242,74,190,35,127,217,103,248,175,255,0,173,21,241,14,191,165,63,218,215,254,78,99,225,159,253,144,31,218,139,255,0,86,15,236,135,95,205,103,237,79,255,0,36,171,226,55,253,150,127,138,255,0,250,209,95,16,235,232,48,63,99,210,63,250,84,142,121,239,243,253,17,245,7,236,187,255,0,30,127,178,15,253,142,254,31,255,0,214,98,248,179,95,176,7,254,77,195,254,10,249,255,0,98,151,197,159,253,119,103,193,10,252,127,253,151,127,227,207,246,65,255,0,177,223,195,255,0,250,204,95,22,107,246,0,255,0,201,184,127,193,95,63,236,82,248,179,255,0,174,236,248,33,89,98,254,47,156,127,244,236,70,183,143,245,209,31,189,190,55,255,0,146,235,255,0,4,211,255,0,179,196,241,167,254,187,227,246,244,173,239,138,191,242,102,95,240,114,159,253,130,127,104,143,253,114,119,236,103,88,62,55,255,0,146,235,255,0,4,211,255,0,179,196,241,167,254,187,227,246,244,173,239,138,191,242,102,95,240,114,159,253,130,127,104,143,253,114,119,236,103,95,62,246,135,170,255,0,212,152,155,246,244,127,250,74,62,189,253,162,127,229,36,127,178,151,253,153,7,252,20,67,255,0,87,207,252,19,70,191,9,190,50,255,0,202,46,63,224,160,127,246,54,127,193,101,191,245,170,63,108,42,253,217,253,162,127,229,36,127,178,151,253,153,7,252,20,67,255,0,87,207,252,19,70,191,9,190,50,255,0,202,46,63,224,160,127,246,54,127,193,101,191,245,170,63,108,42,214,143,252,186,245,165,255,0,167,42,3,219,229,255,0,200,159,33,254,214,191,242,115,31,12,255,0,236,128,254,212,95,250,176,127,100,58,254,107,63,106,127,249,37,95,17,191,236,179,252,87,255,0,214,138,248,135,95,210,159,237,107,255,0,39,49,240,207,254,200,15,237,69,255,0,171,7,246,67,175,230,179,246,167,255,0,146,85,241,27,254,203,63,197,127,253,104,175,136,117,244,24,31,177,233,31,253,42,71,60,247,249,254,136,250,131,246,93,255,0,143,63,217,7,254,199,127,15,255,0,235,49,124,89,175,216,3,255,0,38,225,255,0,5,124,255,0,177,75,226,207,254,187,179,224,133,126,50,254,205,111,241,6,222,211,224,56,211,254,13,120,247,94,31,12,124,67,162,107,26,243,105,122,231,194,27,113,121,107,31,193,207,26,124,63,84,209,198,187,241,70,200,207,59,235,30,42,211,101,81,56,183,81,106,146,187,186,204,171,3,254,189,104,86,95,180,23,137,254,18,254,221,126,3,208,255,0,100,111,141,183,186,191,237,69,225,239,27,217,124,62,185,62,53,253,149,45,180,221,37,252,77,251,41,124,60,248,25,167,203,227,9,110,255,0,105,56,229,211,161,79,22,248,59,84,184,155,236,80,234,14,186,116,208,76,137,37,211,61,156,121,226,210,230,214,113,90,173,231,21,181,72,190,175,182,163,93,26,87,183,249,47,35,250,13,241,191,252,151,95,248,38,159,253,158,39,141,63,245,223,31,183,165,111,124,85,255,0,147,50,255,0,131,148,255,0,236,19,251,68,127,235,147,191,99,58,249,170,227,226,39,237,23,227,159,137,95,178,7,139,252,61,255,0,4,252,253,170,174,52,223,128,223,30,53,255,0,138,190,46,183,159,199,223,176,109,173,254,169,225,221,95,246,92,253,166,190,7,219,90,120,102,57,191,109,81,29,222,178,190,44,248,203,225,139,135,142,238,91,40,6,157,167,223,202,46,26,230,43,107,75,175,89,241,37,191,237,141,227,175,217,247,254,10,229,240,175,65,255,0,130,110,254,213,79,226,15,219,234,203,226,181,167,193,219,219,191,138,223,240,78,219,109,27,195,82,248,231,254,9,227,251,63,254,201,90,79,252,44,201,207,237,216,110,52,85,143,226,55,194,175,17,95,92,255,0,102,91,107,5,52,59,187,59,168,132,215,207,113,166,219,120,46,31,5,234,211,86,107,254,94,83,255,0,159,234,95,205,252,186,255,0,193,54,187,236,254,231,217,121,31,124,254,209,63,242,146,63,217,75,254,204,131,254,10,33,255,0,171,231,254,9,163,95,132,223,25,127,229,23,31,240,80,63,251,27,63,224,178,223,250,213,31,182,21,126,168,124,102,248,157,251,80,248,143,246,183,248,37,241,230,31,248,38,167,237,133,167,248,71,225,191,236,235,251,85,124,31,214,244,205,71,226,103,252,19,143,254,18,107,191,19,124,113,248,153,251,33,120,203,194,119,186,77,142,157,251,124,220,91,92,104,118,218,103,192,31,25,71,169,75,53,228,19,195,113,168,233,169,109,109,119,28,183,114,217,126,69,124,89,210,127,106,155,143,216,203,246,168,253,157,238,63,97,31,218,43,77,241,167,198,93,111,246,253,212,252,53,170,222,124,69,253,136,228,240,182,155,7,237,85,241,167,227,223,196,127,135,137,226,11,221,59,246,189,184,187,182,150,207,67,248,159,160,197,172,139,91,27,197,183,187,179,188,75,22,212,97,142,9,238,46,148,116,167,251,202,119,94,207,106,144,118,180,228,223,218,236,215,222,18,122,108,210,244,126,94,71,205,31,181,175,252,156,199,195,63,251,32,63,181,23,254,172,31,217,14,191,154,207,218,159,254,73,87,196,111,251,44,255,0,21,255,0,245,162,190,33,215,244,37,251,73,107,95,27,124,67,241,127,194,255,0,16,79,236,147,241,207,69,209,124,43,240,191,227,47,130,245,75,109,91,197,255,0,178,171,234,71,84,248,137,226,159,129,122,214,139,113,109,14,141,251,74,94,69,46,159,21,175,194,239,16,173,211,153,150,68,150,234,205,98,138,117,146,103,183,254,127,63,105,95,11,252,75,188,240,95,139,124,53,127,240,179,197,58,5,254,185,227,223,26,120,190,210,77,87,93,248,105,61,172,122,95,138,62,45,120,151,198,218,116,87,50,104,126,62,188,100,188,93,47,196,86,17,76,170,142,139,114,178,34,200,241,40,153,189,252,29,162,225,121,199,236,253,168,189,155,125,31,154,48,150,186,219,250,208,253,123,241,71,194,211,251,40,126,216,127,180,159,236,197,29,183,140,244,189,39,225,23,198,159,136,254,4,240,64,248,139,164,201,103,227,109,99,192,90,15,138,174,207,194,159,21,106,239,14,131,167,91,234,63,219,31,13,91,193,154,197,182,161,107,99,109,167,234,150,158,35,143,80,178,139,236,151,48,21,253,111,253,159,124,79,166,71,107,164,76,36,119,22,225,35,117,1,11,110,155,201,48,187,34,187,52,113,54,213,193,40,8,243,20,149,231,143,107,255,0,131,146,127,96,95,16,124,62,248,163,165,127,193,75,190,22,111,155,194,222,58,147,192,31,11,255,0,104,223,15,216,88,120,199,82,212,180,95,28,233,122,93,223,134,190,31,252,96,212,181,89,110,175,180,157,15,192,186,175,132,244,63,7,120,38,250,38,135,69,182,180,215,52,159,11,11,120,245,157,79,197,119,178,88,254,76,254,206,255,0,24,99,150,222,194,236,204,255,0,100,186,0,92,192,93,31,201,186,148,152,252,182,72,217,147,126,223,48,149,67,19,23,46,2,130,232,181,205,152,225,156,100,218,94,235,213,21,7,163,93,127,225,143,233,207,225,31,139,237,160,91,72,227,116,15,110,232,34,108,3,190,50,99,87,109,196,54,214,202,174,20,166,114,172,156,176,4,125,249,225,31,137,118,194,0,208,94,8,203,47,152,240,205,242,198,196,42,169,98,160,30,67,72,65,224,124,201,158,48,69,127,63,223,13,254,47,219,199,5,180,70,243,125,178,44,66,27,164,144,75,52,104,229,1,45,243,159,45,54,201,4,132,177,96,24,103,247,108,64,95,175,180,79,140,192,17,27,221,65,59,11,118,103,255,0,72,17,205,184,200,140,197,223,114,140,2,72,198,246,232,62,246,51,95,47,91,15,38,245,95,129,180,103,109,207,210,255,0,26,252,74,182,120,230,223,118,102,151,107,36,104,159,52,81,180,161,88,5,194,227,27,102,99,202,156,42,16,1,234,63,60,62,45,248,186,209,227,149,100,40,204,55,92,188,138,227,27,208,110,216,4,97,139,150,17,46,8,69,28,59,12,174,49,200,120,143,227,50,200,151,136,111,98,183,10,129,152,199,42,188,226,63,33,124,205,206,220,72,54,51,54,60,195,140,12,100,128,149,241,111,197,15,140,22,211,90,220,172,183,34,27,77,166,66,165,200,154,121,2,171,4,123,117,117,102,81,190,8,192,224,16,237,181,2,156,171,195,225,228,164,146,95,214,129,41,221,91,161,243,175,237,13,226,59,3,103,121,228,74,203,61,204,207,61,169,43,25,100,56,141,33,121,35,222,89,1,149,83,25,82,62,117,221,128,78,62,70,255,0,130,119,126,207,31,15,191,110,31,248,41,223,193,175,128,255,0,23,126,29,235,95,22,254,5,220,233,127,21,117,191,139,126,30,210,27,199,90,22,159,164,248,87,194,191,9,60,101,113,225,253,127,196,190,44,240,37,253,134,163,225,77,21,62,45,92,124,44,182,75,182,212,44,224,185,212,245,171,13,40,188,163,80,22,115,249,223,237,31,241,136,164,87,183,2,87,145,167,105,96,211,109,124,216,190,101,185,87,142,73,80,58,170,29,178,6,201,72,229,194,135,108,149,100,106,254,150,255,0,224,219,79,248,39,214,187,240,159,225,199,137,191,224,160,31,22,119,143,31,126,211,62,13,139,194,127,6,180,11,219,15,25,104,218,239,131,190,5,65,226,185,117,109,111,196,218,244,26,173,237,190,159,173,127,194,123,226,15,12,248,39,90,209,165,131,76,186,138,31,11,248,75,64,213,52,189,114,88,188,79,168,233,246,127,83,150,225,229,205,25,63,134,59,254,6,19,122,36,158,135,244,231,226,95,13,120,115,198,158,28,241,7,131,188,99,225,253,19,197,158,17,241,102,137,170,248,107,197,62,22,241,46,149,97,174,248,115,196,190,28,215,108,39,210,245,207,15,248,131,67,213,32,150,215,89,209,47,52,203,171,171,107,187,75,152,164,130,226,11,153,33,154,55,141,217,79,249,228,127,193,69,191,224,145,127,180,135,252,18,210,127,16,124,97,240,127,136,7,198,63,216,238,239,198,207,164,104,63,16,98,105,91,226,7,195,125,35,85,254,197,62,17,211,127,104,221,46,13,2,222,199,74,154,231,91,189,185,208,172,188,75,165,188,250,46,177,121,165,65,253,161,111,225,93,95,196,122,38,131,117,254,138,84,87,185,86,148,106,199,150,68,38,214,199,249,137,124,35,253,164,225,17,42,65,168,27,57,55,91,193,37,149,203,249,66,9,166,10,177,121,59,164,204,204,18,89,163,27,137,140,249,57,14,128,0,62,190,209,191,104,196,37,174,101,66,145,60,111,28,127,102,46,100,118,15,30,24,73,107,149,104,254,87,82,55,231,114,143,151,140,215,239,231,237,97,255,0,6,209,126,197,31,27,117,111,19,248,223,246,122,241,79,142,191,99,239,30,235,178,91,223,90,232,190,8,183,211,124,117,240,10,207,94,186,241,181,239,137,252,75,174,159,131,158,34,150,218,255,0,67,23,122,62,171,125,166,89,233,94,22,241,95,133,252,61,163,166,159,165,203,97,163,36,118,119,54,186,135,242,69,255,0,5,83,253,146,188,91,255,0,4,167,253,161,124,29,251,60,127,194,237,159,227,167,252,38,95,6,124,61,241,163,254,18,232,60,17,47,194,216,244,227,226,47,27,252,68,240,63,252,35,135,195,215,94,54,241,67,93,180,63,240,174,126,211,246,193,127,8,147,251,92,66,44,227,54,230,123,159,30,182,88,239,120,217,175,184,181,62,232,251,87,92,253,162,136,121,164,139,9,29,194,121,81,79,41,11,44,114,121,42,170,204,247,101,75,182,224,229,66,185,56,140,242,184,21,241,31,198,31,218,90,22,89,164,23,209,222,220,189,172,147,193,8,147,125,140,42,167,113,158,107,134,108,199,31,218,30,73,14,240,20,125,144,129,27,97,26,189,179,254,9,29,255,0,4,228,241,111,252,21,216,126,208,36,254,210,18,252,2,255,0,134,124,255,0,133,83,129,113,240,194,227,226,200,241,103,252,45,143,248,89,32,236,22,191,19,124,41,253,129,246,15,248,86,156,238,251,127,218,191,182,70,62,203,246,99,246,143,235,107,246,30,255,0,131,126,127,98,47,216,227,198,94,8,248,195,175,75,227,175,218,75,227,167,129,191,225,13,215,188,61,227,63,139,119,218,93,175,129,252,19,241,35,195,122,94,173,111,170,120,227,225,183,194,95,9,233,214,122,126,155,45,206,181,171,255,0,104,105,169,226,123,143,23,234,62,28,185,240,254,143,117,163,107,22,250,165,131,234,151,78,134,91,170,148,157,151,222,55,81,116,71,224,215,252,18,131,254,8,163,241,175,246,186,241,143,193,15,219,43,246,172,254,199,240,143,236,169,103,173,217,252,71,240,167,194,239,18,233,86,250,215,196,31,218,27,75,240,221,239,135,117,111,4,91,235,30,15,241,39,134,110,52,157,55,246,111,241,36,203,118,247,215,90,155,207,168,120,159,69,209,230,131,74,209,32,208,124,79,165,120,185,127,187,106,40,175,98,157,56,210,138,140,21,146,51,109,183,118,127,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 6364; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

