#ifndef GETVESUITE_HTRIXIST_HTRIXIST_E-HS-1CN_H
#define GETVESUITE_HTRIXIST_HTRIXIST_E-HS-1CN_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_HTRIXIST_HTRIXIST_E-HS-1CN( void )
{
    unsigned char osgData[ 4872 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,53,0,146,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,234,255,0,107,79,248,42,223,252,19,255,0,246,217,213,100,248,253,224,15,217,199,226,87,130,60,23,226,59,63,217,175,88,253,168,126,9,124,81,248,35,240,166,219,199,127,240,81,93,51,193,255,0,183,167,252,19,163,227,63,130,103,214,108,124,3,227,143,17,120,63,226,226,248,15,246,81,253,155,127,105,205,10,213,126,46,248,155,195,109,167,217,124,113,182,240,223,133,70,161,99,226,15,19,199,166,126,154,93,127,193,218,191,176,149,149,159,138,181,11,175,217,55,254,10,23,21,159,130,62,209,255,0,9,76,231,225,239,236,178,203,164,253,147,65,176,241,61,199,152,169,251,94,150,184,219,161,106,118,55,7,201,18,101,110,2,12,200,25,7,226,247,236,131,251,46,159,16,126,201,191,178,246,189,253,137,36,223,219,127,179,191,193,77,95,205,22,178,184,148,106,95,13,124,51,120,36,12,34,249,129,19,100,30,115,158,166,190,120,248,165,251,58,125,135,224,191,252,21,115,82,58,60,139,255,0,8,39,252,44,99,191,236,210,1,107,246,63,216,19,224,47,138,254,99,229,126,236,143,237,80,252,145,196,155,186,28,215,143,79,50,139,171,90,10,42,241,146,191,254,5,24,126,171,230,91,138,75,125,87,249,92,255,0,68,159,217,203,246,160,253,158,191,107,175,134,182,95,23,127,102,159,139,254,7,248,203,240,254,238,123,109,58,239,90,240,94,179,13,253,215,134,60,69,62,129,161,248,158,227,193,30,62,240,244,194,61,79,225,199,196,123,45,19,196,186,12,186,175,134,117,251,61,55,95,209,219,84,138,13,87,77,179,184,38,33,243,167,194,223,248,40,103,131,190,40,252,122,240,63,236,199,31,192,95,218,11,193,95,27,181,132,248,238,255,0,22,60,25,227,20,248,15,34,254,205,11,240,27,193,255,0,179,79,143,117,3,241,131,196,158,12,248,239,172,233,62,38,255,0,132,131,194,159,182,55,236,205,117,225,255,0,248,87,55,222,61,199,252,45,104,160,215,255,0,176,46,116,111,18,195,162,255,0,20,191,7,116,143,218,199,246,53,248,153,226,239,30,254,199,95,23,188,75,240,75,196,222,57,240,228,94,20,241,226,232,250,23,132,252,85,225,239,24,232,90,103,136,46,245,173,5,188,67,224,191,136,30,22,214,244,93,71,89,210,181,11,141,80,105,58,156,186,105,213,52,152,60,85,173,90,105,183,214,118,154,230,179,6,161,235,49,254,208,63,240,81,141,47,226,62,155,241,211,194,63,19,52,95,4,126,209,82,175,197,228,248,137,241,239,66,248,103,224,237,75,198,95,24,211,227,46,137,251,45,120,75,94,30,58,240,111,140,188,43,170,248,35,66,251,7,130,127,98,239,217,187,70,210,255,0,225,17,240,159,134,54,218,124,62,123,171,255,0,237,13,95,87,214,245,61,71,117,153,97,223,45,229,202,250,233,221,127,157,131,145,159,232,9,69,127,8,255,0,240,223,191,240,90,207,250,61,13,115,255,0,17,247,246,87,255,0,232,118,163,254,27,247,254,11,89,255,0,71,163,174,127,226,62,254,202,223,253,14,181,95,218,88,95,230,127,119,252,31,48,228,145,253,220,81,95,194,63,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,218,88,95,230,127,119,252,31,48,228,145,253,220,81,95,194,63,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,218,88,95,230,127,119,252,31,48,228,145,253,220,81,95,194,63,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,218,88,95,230,127,119,252,31,48,228,145,253,220,81,95,194,63,252,55,239,252,22,179,254,143,71,92,255,0,196,125,253,149,191,250,29,104,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,31,218,88,95,230,127,119,252,31,48,228,145,253,220,87,63,226,207,22,120,87,192,94,21,241,55,142,188,117,226,111,15,248,47,193,62,11,240,254,179,226,207,24,248,199,197,154,206,157,225,207,10,248,79,194,190,28,211,174,117,143,16,248,155,196,222,33,214,46,97,180,208,124,63,97,164,89,222,93,94,222,221,77,21,181,173,181,172,147,207,34,68,140,195,248,103,255,0,134,253,255,0,130,214,127,209,232,235,159,248,143,191,178,183,255,0,67,173,120,31,237,3,241,123,254,10,73,251,94,120,115,68,248,61,251,83,254,211,254,46,248,149,240,105,252,67,165,248,187,86,248,115,7,128,254,17,124,59,208,188,81,226,47,10,234,90,125,223,133,38,241,116,159,11,126,23,248,126,235,197,154,70,153,170,204,186,157,174,151,169,93,93,233,17,235,26,94,151,173,54,158,218,198,143,163,95,233,201,230,88,84,190,55,123,109,111,235,184,114,72,253,225,248,205,255,0,7,79,127,193,58,254,20,248,247,226,223,132,188,45,240,227,246,174,253,161,124,35,240,115,88,190,209,53,223,141,255,0,1,188,27,240,67,90,248,53,226,107,141,31,195,154,103,136,245,205,71,225,246,191,227,255,0,218,15,195,154,151,140,252,49,109,6,167,229,193,172,218,233,7,72,214,150,215,251,71,195,183,218,198,137,117,167,106,183,188,215,252,69,85,251,25,127,209,158,127,193,67,191,240,132,253,148,63,250,49,43,249,128,186,253,155,163,210,191,102,15,248,41,221,228,90,83,24,252,1,105,241,37,86,101,129,182,91,125,143,246,14,248,25,226,172,238,216,66,97,181,82,231,145,141,251,184,206,107,245,27,254,25,25,179,255,0,32,9,72,255,0,175,73,112,113,211,254,88,116,255,0,10,231,171,154,170,106,45,69,62,103,255,0,182,193,255,0,237,193,24,222,247,123,31,98,234,255,0,240,122,7,252,19,11,65,213,181,77,15,86,253,158,191,224,160,54,122,174,141,168,222,233,58,157,155,252,47,253,156,36,123,93,67,78,185,150,206,246,217,164,135,246,173,100,145,163,185,134,85,44,140,202,74,101,88,140,26,43,252,223,191,107,221,28,120,127,246,178,253,168,116,31,47,202,254,196,253,162,126,53,233,30,81,27,76,95,217,191,18,188,77,101,229,149,44,54,149,242,113,140,12,99,165,21,234,198,105,168,190,233,126,54,255,0,49,114,187,219,250,233,254,103,250,150,127,193,55,190,4,182,175,255,0,4,239,253,130,181,81,96,28,106,127,177,127,236,183,168,135,219,31,206,47,126,7,120,22,231,121,201,234,124,220,254,53,241,135,199,223,131,111,105,251,48,127,193,194,183,159,98,3,254,17,65,241,171,45,182,63,220,253,159,254,8,255,0,251,34,235,252,97,189,47,247,113,253,236,242,79,31,175,159,240,75,223,137,190,31,211,255,0,224,153,255,0,240,78,235,9,133,177,154,203,246,23,253,146,109,37,45,27,22,243,109,254,0,252,63,134,76,157,188,157,232,107,225,111,218,51,226,22,137,63,236,155,255,0,7,41,219,162,219,239,215,63,225,123,253,143,17,156,254,251,254,8,163,251,23,105,105,179,43,199,250,77,172,159,137,62,196,124,13,10,149,62,191,139,79,110,117,255,0,167,233,255,0,153,211,100,225,11,174,159,251,105,219,248,71,246,93,131,89,241,77,196,83,105,171,46,52,41,167,62,100,34,76,176,212,109,148,177,221,3,228,230,83,219,191,94,199,213,127,225,142,172,191,232,17,23,167,252,122,167,111,165,135,249,245,61,107,234,79,135,63,21,124,45,164,248,162,249,222,29,62,87,26,52,176,111,156,238,202,201,119,167,77,180,36,211,252,189,14,57,39,175,169,199,184,127,194,243,240,159,252,250,232,223,247,238,219,255,0,146,58,87,157,58,245,185,182,127,151,111,248,6,156,177,236,126,117,255,0,195,29,89,127,208,34,47,252,5,95,254,64,163,254,24,234,203,254,129,17,127,224,42,255,0,242,5,126,138,127,194,243,240,159,252,251,104,223,247,197,183,255,0,36,81,255,0,11,207,194,127,243,237,163,127,223,22,223,252,145,81,237,171,233,100,254,255,0,79,242,95,120,114,199,177,249,215,255,0,12,117,101,255,0,64,136,191,240,21,127,249,2,143,248,99,171,47,250,4,69,255,0,128,171,255,0,200,21,250,41,255,0,11,207,194,127,243,237,163,127,223,22,223,252,145,71,252,47,63,9,255,0,207,182,141,255,0,124,91,127,242,69,30,218,190,150,79,239,244,255,0,37,247,135,44,123,31,157,127,240,199,86,95,244,8,139,255,0,1,87,255,0,144,40,255,0,134,58,178,255,0,160,68,95,248,10,191,252,129,95,162,159,240,188,252,39,255,0,62,218,55,253,241,109,255,0,201,20,127,194,243,240,159,252,251,104,223,247,197,183,255,0,36,81,237,171,233,100,254,255,0,79,242,95,120,114,199,177,249,215,255,0,12,117,101,255,0,64,136,191,240,21,127,249,2,143,248,99,171,47,250,4,69,255,0,128,171,255,0,200,21,250,41,255,0,11,207,194,127,243,237,163,127,223,22,223,252,145,71,252,47,63,9,255,0,207,182,141,255,0,124,91,127,242,69,30,218,190,150,79,239,244,255,0,37,247,135,44,123,31,157,127,240,199,86,95,244,8,139,255,0,1,87,255,0,144,40,255,0,134,58,178,255,0,160,68,95,248,10,191,252,129,95,162,159,240,188,252,39,255,0,62,218,55,253,241,109,255,0,201,20,127,194,243,240,159,252,251,104,223,247,197,183,255,0,36,81,237,171,233,100,254,255,0,79,242,95,120,114,199,177,249,215,255,0,12,117,101,255,0,64,136,191,240,21,127,249,2,188,179,226,55,236,161,105,164,95,248,94,101,211,85,26,89,117,4,194,192,136,8,19,105,13,243,127,163,38,72,32,99,174,3,30,153,175,214,127,248,94,126,19,255,0,159,109,27,254,248,182,255,0,228,138,241,239,139,31,23,252,41,168,159,15,184,180,211,9,182,58,161,83,7,150,140,166,79,236,211,146,98,156,159,249,102,49,244,226,170,21,171,57,69,89,219,254,27,254,0,185,99,216,254,124,124,99,240,104,218,126,197,63,240,95,155,193,102,54,248,73,126,61,168,114,6,98,22,159,240,73,95,217,75,94,10,11,28,128,171,126,164,1,208,17,138,253,219,255,0,134,121,127,250,6,143,251,230,48,127,244,46,43,242,255,0,226,79,142,244,88,255,0,97,223,248,56,247,79,95,37,164,214,127,225,161,190,200,118,177,111,244,143,248,35,47,236,119,166,166,194,89,177,254,145,111,39,241,112,115,244,175,232,27,254,22,199,134,249,194,218,158,57,253,219,116,30,163,111,29,171,183,25,82,167,179,195,217,117,87,255,0,193,84,5,20,181,86,211,254,11,63,198,39,254,10,67,167,54,143,255,0,5,16,253,189,116,157,161,63,178,255,0,109,15,218,147,78,217,143,185,246,47,142,62,58,182,219,215,183,149,143,194,138,218,255,0,130,161,93,69,127,255,0,5,47,255,0,130,136,223,194,84,67,123,251,116,126,214,247,113,96,16,60,171,159,143,223,16,38,143,3,28,13,174,40,175,188,166,223,179,131,242,95,146,56,238,255,0,171,159,222,231,236,25,251,75,88,104,63,176,215,236,99,161,189,220,170,250,55,236,161,251,59,105,78,161,162,194,182,159,240,135,193,246,140,6,103,28,102,19,218,190,81,248,211,251,64,89,94,254,207,63,240,91,253,61,110,101,45,227,127,248,91,62,80,45,22,36,251,95,252,18,247,246,98,240,192,221,251,254,127,121,166,50,240,15,3,215,129,248,35,240,67,254,10,69,224,207,3,124,22,248,67,224,155,175,17,104,240,93,120,59,225,127,128,60,45,113,4,154,253,172,50,67,63,135,188,41,164,233,50,197,36,44,185,137,214,75,70,5,79,42,65,7,165,121,143,140,63,111,143,10,107,62,2,253,178,252,63,22,189,165,60,191,26,127,225,41,58,108,107,173,91,59,94,127,106,254,205,159,14,62,25,39,217,144,46,110,139,94,248,106,104,134,220,101,227,49,245,83,94,37,60,186,113,196,214,169,200,215,60,175,127,251,137,9,126,134,174,122,36,165,183,249,31,213,118,189,251,100,219,120,87,89,55,51,107,49,91,199,113,11,219,252,215,41,16,103,242,236,165,81,187,206,76,29,170,252,6,231,113,36,28,2,51,7,252,20,7,77,63,243,48,219,255,0,224,197,15,235,253,163,205,122,151,252,19,111,254,8,77,241,79,227,238,169,161,126,208,191,240,82,93,22,243,193,255,0,5,117,127,9,183,136,62,25,126,202,118,190,44,248,135,224,191,141,154,230,167,226,40,181,13,39,76,213,255,0,105,11,191,15,38,137,121,240,122,219,79,240,244,22,90,197,143,132,244,189,86,235,196,55,55,190,41,176,139,197,242,248,66,247,195,154,231,131,53,175,217,207,248,112,63,252,18,123,254,141,151,196,95,248,147,159,181,191,255,0,63,122,168,100,124,202,243,106,47,183,221,216,94,211,94,172,252,17,255,0,135,128,105,159,244,48,219,255,0,224,193,63,249,99,71,252,60,3,76,255,0,161,134,223,255,0,6,9,255,0,203,26,253,238,255,0,135,3,255,0,193,39,191,232,217,124,69,255,0,137,57,251,91,255,0,243,247,163,254,28,15,255,0,4,158,255,0,163,101,241,23,254,36,231,237,111,255,0,207,222,171,251,10,31,204,191,31,47,242,15,105,234,126,8,255,0,195,192,52,207,250,24,109,255,0,240,96,159,252,177,163,254,30,1,166,127,208,195,111,255,0,131,4,255,0,229,141,126,247,127,195,129,255,0,224,147,223,244,108,190,34,255,0,196,156,253,173,255,0,249,251,209,255,0,14,7,255,0,130,79,127,209,178,248,139,255,0,18,115,246,183,255,0,231,239,66,200,161,252,203,241,242,255,0,32,246,158,167,224,143,252,60,3,76,255,0,161,134,223,255,0,6,9,255,0,203,26,63,225,224,26,103,253,12,54,255,0,248,48,79,254,88,215,239,119,252,56,31,254,9,61,255,0,70,203,226,47,252,73,207,218,223,255,0,159,189,31,240,224,127,248,36,247,253,27,47,136,191,241,39,63,107,127,254,126,244,44,138,31,204,191,31,47,242,15,105,234,126,8,255,0,195,192,52,207,250,24,109,255,0,240,96,159,252,177,163,254,30,1,166,127,208,195,111,255,0,131,4,255,0,229,141,126,247,127,195,129,255,0,224,147,223,244,108,190,34,255,0,196,156,253,173,255,0,249,251,209,255,0,14,7,255,0,130,79,127,209,178,248,139,255,0,18,115,246,183,255,0,231,239,66,200,161,252,203,241,242,255,0,32,246,158,167,224,143,252,60,3,76,255,0,161,134,223,255,0,6,9,255,0,203,26,63,225,224,26,103,253,12,54,255,0,248,48,79,254,88,215,239,119,252,56,31,254,9,61,255,0,70,203,226,47,252,73,207,218,223,255,0,159,189,31,240,224,127,248,36,247,253,27,47,136,191,241,39,63,107,127,254,126,244,44,138,31,204,191,31,47,242,15,105,234,126,8,255,0,195,192,52,207,250,24,109,255,0,240,96,159,252,177,170,237,251,109,90,248,150,250,198,11,125,114,25,86,15,181,121,202,183,41,55,18,196,29,50,194,230,79,47,152,9,31,119,61,242,7,31,88,254,208,127,240,77,255,0,248,39,175,195,63,140,223,23,124,27,224,47,217,19,66,241,31,195,207,134,63,16,191,224,144,255,0,9,110,60,85,121,251,67,126,219,122,191,246,111,197,47,219,167,246,231,212,126,7,126,209,223,13,124,73,174,104,223,181,21,189,141,159,143,252,55,251,52,248,191,246,121,241,134,139,163,52,86,250,142,143,255,0,11,175,64,241,6,187,109,171,232,30,36,208,237,37,246,175,219,139,254,13,212,248,79,226,31,0,104,30,33,255,0,130,110,77,166,126,207,191,29,60,41,170,206,183,190,25,248,187,241,103,227,111,140,254,13,124,92,240,182,189,38,157,103,169,105,254,42,241,39,138,110,252,107,175,124,53,241,70,139,29,188,154,142,137,169,232,86,23,118,87,190,110,163,162,107,122,45,201,213,52,175,17,248,74,22,75,31,125,66,73,202,155,179,186,106,239,150,47,71,181,172,214,189,238,186,7,180,219,115,241,143,198,127,31,109,110,63,101,47,248,45,102,150,247,76,210,120,215,254,23,55,150,21,144,172,134,243,254,9,153,251,54,248,101,51,153,50,126,125,52,169,192,63,119,191,111,216,127,248,106,189,52,255,0,203,228,199,57,239,23,234,62,211,239,95,196,223,199,127,143,62,58,253,156,87,246,243,253,151,127,104,15,10,107,191,9,62,54,235,58,175,139,252,63,226,239,134,254,58,158,207,78,241,46,149,173,107,63,179,95,195,95,3,194,176,37,164,211,218,248,151,67,188,93,42,27,157,31,87,210,238,175,116,109,119,76,189,179,213,116,77,71,82,210,175,108,239,174,62,169,31,183,150,178,126,22,127,194,243,255,0,132,111,196,167,224,159,252,39,255,0,240,169,255,0,225,113,127,103,234,255,0,240,170,255,0,225,106,127,194,57,255,0,9,135,252,43,79,248,88,63,217,159,217,63,240,159,255,0,194,39,141,83,251,27,237,159,218,63,217,191,233,223,102,251,55,239,43,44,78,89,57,198,154,112,111,149,175,253,34,154,252,208,227,43,55,121,104,252,252,222,199,243,231,251,121,107,75,174,254,220,159,182,110,184,140,236,154,207,237,95,251,68,234,170,196,242,203,168,252,95,241,133,226,177,195,30,72,152,81,94,53,241,191,197,48,120,231,227,79,197,239,27,90,202,147,219,120,199,226,135,143,252,83,111,60,82,44,209,205,7,136,60,87,171,106,208,203,28,203,196,168,209,221,169,12,56,96,217,29,104,175,165,132,18,140,85,182,75,244,255,0,47,204,196,255,0,122,138,40,162,180,0,162,138,40,0,162,138,40,0,162,138,40,0,162,138,40,0,162,138,40,0,162,138,40,0,162,138,40,0,175,228,11,246,179,253,184,215,194,127,240,93,155,57,15,194,223,237,7,253,159,63,111,207,248,34,79,236,42,167,254,19,115,105,255,0,9,98,254,216,95,177,239,252,22,61,155,226,134,63,225,18,144,104,63,240,143,31,219,125,63,226,73,254,155,253,173,255,0,10,187,141,95,76,254,219,63,217,37,20,1,253,126,209,69,20,1,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 4872; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

