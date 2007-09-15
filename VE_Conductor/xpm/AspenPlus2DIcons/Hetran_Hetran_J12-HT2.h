#ifndef GETVESUITE_Hetran_Hetran_J12-HT2_H
#define GETVESUITE_Hetran_Hetran_J12-HT2_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Hetran_Hetran_J12-HT2( void )
{
    unsigned char osgData[ 5311 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,60,0,142,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,250,39,246,161,255,0,130,205,126,200,127,183,167,195,255,0,218,155,199,191,12,116,143,141,127,179,212,159,18,63,224,146,255,0,182,199,236,185,103,225,63,141,95,14,237,52,255,0,20,126,213,127,20,126,60,105,127,15,117,239,217,14,13,50,231,224,23,137,124,111,163,221,120,119,192,231,76,253,162,173,161,189,248,129,171,120,105,124,47,115,251,82,220,183,134,227,186,181,241,31,141,174,180,207,185,236,191,224,235,255,0,216,139,81,185,213,237,44,255,0,100,95,248,40,100,247,58,14,161,30,149,171,68,60,3,251,42,33,179,212,38,210,116,205,114,59,103,105,63,108,16,36,99,165,107,58,100,219,144,178,226,232,41,109,234,234,191,137,191,178,31,236,186,124,65,251,38,254,203,250,255,0,246,28,179,13,111,246,119,248,43,171,249,162,210,86,18,157,75,225,183,134,175,124,192,194,19,187,62,113,57,7,156,210,252,20,253,151,78,165,241,43,246,188,178,254,196,149,255,0,225,30,253,162,60,53,164,108,251,36,167,202,243,255,0,100,239,217,127,94,242,240,34,249,56,214,195,140,224,254,244,28,16,121,240,255,0,180,148,106,98,90,138,110,47,93,123,56,199,245,53,80,186,143,159,234,127,111,255,0,176,231,237,197,251,59,255,0,193,67,255,0,103,79,5,254,211,159,179,63,139,101,241,31,128,252,89,18,89,234,250,30,179,111,111,164,248,251,225,151,141,45,244,237,55,82,215,62,25,252,80,240,188,55,183,7,194,222,59,211,173,181,125,50,89,97,89,238,108,111,236,53,109,63,93,208,239,245,111,14,234,218,70,175,127,245,213,127,154,191,252,18,191,246,170,253,179,255,0,224,159,159,15,126,29,106,191,179,13,231,132,117,223,3,252,122,248,63,240,211,89,241,167,194,15,138,186,63,137,124,85,240,174,239,198,240,124,53,240,157,221,135,197,205,19,70,240,191,138,52,139,255,0,13,252,77,95,14,233,49,104,183,215,182,58,132,86,218,214,148,182,150,254,32,178,213,36,208,60,45,55,135,255,0,98,255,0,225,248,255,0,240,85,79,250,35,159,177,183,254,27,31,143,127,252,255,0,43,209,120,236,52,93,165,62,87,219,231,98,57,101,216,254,199,104,175,227,139,254,31,143,255,0,5,84,255,0,162,57,251,27,127,225,178,248,245,255,0,207,242,143,248,126,63,252,21,83,254,136,231,236,109,255,0,134,203,227,215,255,0,63,202,95,95,194,255,0,207,213,248,255,0,93,67,146,93,143,236,118,138,254,56,191,225,248,255,0,240,85,79,250,35,159,177,183,254,27,47,143,95,252,255,0,40,255,0,135,227,255,0,193,85,63,232,142,126,198,223,248,108,190,61,127,243,252,163,235,248,95,249,250,191,31,235,168,114,75,177,253,126,248,179,89,212,124,57,225,95,19,120,135,71,240,159,136,60,123,171,232,62,31,214,117,157,47,192,190,19,185,240,173,159,138,188,105,168,233,122,117,205,245,143,132,252,51,121,227,175,19,104,186,37,175,136,53,27,152,34,179,178,147,89,214,116,157,41,46,111,35,109,67,83,176,180,19,93,69,242,231,236,157,251,108,252,43,253,180,103,248,165,169,252,17,240,247,196,43,207,134,95,13,219,224,138,104,255,0,25,124,75,165,120,107,195,254,6,248,176,223,29,191,103,95,133,159,181,47,135,7,195,189,2,111,22,201,226,251,15,236,239,132,95,27,190,19,220,234,255,0,240,149,120,95,195,62,93,231,140,227,177,211,191,180,231,176,213,134,159,252,210,255,0,195,241,255,0,224,170,159,244,71,63,99,111,252,54,95,30,191,249,254,87,205,223,179,87,237,249,251,95,254,196,154,15,138,254,28,126,202,63,5,126,19,216,124,36,215,164,248,33,38,153,164,126,208,167,199,191,24,62,35,105,77,240,63,246,64,253,154,191,99,173,32,95,248,247,225,189,247,195,205,51,87,23,222,12,253,153,60,41,170,220,249,126,24,178,217,169,248,131,80,88,177,107,246,104,97,95,95,195,115,43,85,92,182,125,58,233,96,228,151,99,251,174,162,191,142,47,248,126,63,252,21,83,254,136,231,236,109,255,0,134,203,227,215,255,0,63,202,63,225,248,255,0,240,85,79,250,35,159,177,183,254,27,47,143,95,252,255,0,41,253,127,11,255,0,63,127,175,233,135,36,187,31,216,237,21,252,113,127,195,241,255,0,224,170,159,244,71,63,99,111,252,54,95,30,191,249,254,81,255,0,15,199,255,0,130,170,127,209,28,253,141,191,240,217,124,122,255,0,231,249,71,215,240,191,243,245,126,63,215,80,228,151,99,251,29,162,191,142,47,248,126,63,252,21,83,254,136,231,236,109,255,0,134,203,227,215,255,0,63,202,249,151,227,207,252,22,59,254,10,83,226,223,138,159,177,78,191,226,47,133,31,178,125,174,175,240,195,246,154,241,95,142,124,9,6,151,240,239,227,93,189,142,163,226,173,67,246,53,253,173,190,26,94,89,248,142,43,207,141,179,73,125,163,47,130,62,33,248,202,226,56,173,36,180,184,26,133,133,132,205,112,214,176,220,218,220,184,227,112,210,118,85,85,245,127,114,191,228,28,178,236,127,118,244,87,243,177,251,56,255,0,193,195,95,9,252,125,241,35,68,248,123,251,80,124,3,215,255,0,101,75,15,24,235,58,70,133,225,95,137,237,241,51,69,248,161,240,183,72,187,191,183,214,218,226,235,226,207,136,110,252,45,225,139,159,135,26,41,212,172,244,11,59,109,66,11,13,106,197,27,91,184,190,215,110,60,63,163,105,119,122,157,127,65,158,26,241,47,135,60,105,225,207,15,248,199,193,222,32,209,60,89,225,31,22,104,154,87,137,124,45,226,159,13,106,182,26,239,135,60,75,225,205,118,194,13,83,67,241,7,135,245,205,46,121,109,117,157,18,243,76,186,181,185,180,187,182,150,72,46,32,185,142,104,100,120,221,88,244,66,164,42,46,104,73,73,121,18,213,183,63,157,159,248,38,247,192,151,213,255,0,224,157,255,0,176,86,172,44,3,141,79,246,47,253,151,53,0,248,140,151,23,191,3,188,13,114,27,36,245,34,92,231,182,115,71,236,185,240,41,239,126,56,255,0,193,72,45,133,128,99,163,254,218,30,6,211,182,237,136,236,243,127,224,158,31,176,94,171,142,79,28,106,161,184,254,255,0,214,190,172,255,0,130,94,124,77,240,253,135,252,19,63,254,9,221,99,50,219,25,172,191,97,111,217,38,210,98,209,177,111,50,219,224,15,195,248,100,220,118,242,119,35,102,143,217,35,226,111,135,237,190,63,127,193,80,167,145,109,138,223,254,221,63,15,238,224,6,54,199,150,159,240,76,255,0,248,39,117,131,109,27,56,95,58,202,95,196,147,208,215,231,115,169,83,219,230,75,118,175,111,252,29,78,199,108,82,229,167,167,69,249,31,131,31,240,77,239,217,214,31,26,252,8,253,133,166,54,10,255,0,218,223,178,255,0,193,173,76,200,98,144,228,75,240,23,78,187,44,222,89,203,23,154,105,136,33,134,2,46,229,230,191,94,207,236,91,7,252,248,199,255,0,126,47,179,249,239,255,0,56,175,0,255,0,130,66,120,227,194,90,119,236,205,255,0,4,250,23,182,182,215,23,86,95,178,55,192,171,105,12,140,216,221,31,236,235,162,192,234,202,93,142,239,48,191,76,12,158,20,87,238,207,252,45,111,4,255,0,208,43,78,255,0,190,100,255,0,227,117,56,234,245,150,38,162,87,118,114,95,249,48,161,20,226,175,174,135,229,39,252,49,108,31,243,227,31,253,248,190,255,0,226,232,255,0,134,45,131,254,124,99,255,0,191,23,223,252,93,126,173,255,0,194,214,240,79,253,2,180,239,251,230,79,254,55,71,252,45,111,4,255,0,208,43,78,255,0,190,100,255,0,227,117,197,245,154,235,163,233,219,200,174,72,246,63,41,63,225,139,96,255,0,159,24,255,0,239,197,247,255,0,23,71,252,49,108,31,243,227,31,253,248,190,255,0,226,235,245,111,254,22,183,130,127,232,21,167,127,223,50,127,241,186,63,225,107,120,39,254,129,90,119,253,243,39,255,0,27,163,235,53,215,71,248,121,7,36,123,31,148,159,240,197,176,127,207,140,127,247,226,251,255,0,139,163,254,24,182,15,249,241,143,254,252,95,127,241,117,250,183,255,0,11,91,193,63,244,10,211,191,239,153,63,248,221,31,240,181,188,19,255,0,64,173,59,254,249,147,255,0,141,209,245,154,233,236,254,245,228,28,145,236,126,82,127,195,22,193,255,0,62,49,255,0,223,139,239,254,46,143,248,98,216,63,231,198,63,251,241,125,255,0,197,215,234,223,252,45,111,4,255,0,208,43,78,255,0,190,100,255,0,227,116,127,194,214,240,79,253,2,180,239,251,230,79,254,55,71,214,107,173,44,244,183,85,228,28,145,236,126,82,127,195,22,193,255,0,62,49,255,0,223,139,239,254,46,143,248,98,216,63,231,198,63,251,241,125,255,0,197,215,234,223,252,45,111,4,255,0,208,43,78,255,0,190,100,255,0,227,116,127,194,214,240,79,253,2,180,239,251,230,79,254,55,71,214,107,174,143,240,242,14,72,246,63,41,63,225,139,96,255,0,159,24,255,0,239,197,247,255,0,23,95,31,126,211,95,178,93,190,139,241,139,254,9,249,108,108,144,174,191,251,93,120,227,73,116,88,110,148,200,150,159,176,47,237,193,226,82,131,204,148,2,127,226,159,7,168,251,185,36,40,53,253,14,127,194,214,240,79,253,2,180,239,251,230,79,254,55,95,4,126,217,63,18,124,37,121,241,191,254,9,143,36,22,86,86,169,167,254,220,127,17,47,110,164,140,56,38,220,127,193,51,127,224,162,86,155,88,144,184,95,62,238,3,146,112,10,130,192,174,65,233,194,226,43,58,147,77,61,41,212,127,117,54,76,161,27,45,58,175,205,31,140,127,181,63,236,143,163,89,105,26,148,87,58,77,184,150,56,188,129,230,195,8,69,69,64,143,25,93,241,172,144,153,30,76,58,40,117,251,86,244,27,128,122,250,43,254,13,242,253,171,254,38,104,127,18,252,123,255,0,4,217,241,38,155,225,237,111,225,103,195,175,134,127,16,126,63,252,27,241,77,132,113,105,26,255,0,129,108,228,248,175,224,203,95,28,124,57,212,173,45,52,180,79,22,232,122,159,138,126,51,62,189,99,125,115,42,106,90,85,234,235,22,114,220,106,154,93,254,141,105,225,191,160,191,107,143,28,248,112,216,235,38,19,109,8,43,44,96,219,73,110,4,174,99,184,141,71,239,4,89,199,156,0,198,236,164,108,195,3,0,255,0,50,151,63,176,223,197,175,248,42,23,237,53,172,126,206,223,0,124,65,240,239,194,30,53,240,159,195,255,0,17,124,103,212,117,79,140,58,175,137,116,15,11,79,225,127,13,120,163,194,94,15,190,176,179,189,240,95,132,188,67,119,39,136,31,83,248,165,225,249,33,138,75,24,173,218,222,206,241,158,233,36,142,24,174,62,131,36,173,89,205,38,155,211,110,230,85,35,21,127,35,245,187,246,12,253,165,236,52,47,216,107,246,49,209,36,187,153,95,70,253,148,63,103,109,41,212,52,32,43,105,223,8,60,31,104,64,221,112,8,195,67,198,71,212,82,254,206,223,180,182,159,167,124,94,253,188,175,26,238,101,26,247,237,95,224,253,86,50,26,44,186,195,251,13,254,198,90,25,98,77,207,93,250,43,142,51,194,243,220,15,231,7,246,72,253,167,62,37,248,171,246,108,211,164,248,89,240,199,226,71,196,223,9,126,204,95,3,254,31,55,198,239,21,252,60,240,151,138,188,101,225,175,131,222,29,208,124,7,114,207,226,15,138,90,223,134,180,59,171,95,135,186,24,211,60,17,226,187,159,181,234,211,90,91,139,127,12,106,19,121,158,93,149,203,71,228,30,0,255,0,130,145,120,51,195,254,42,248,221,171,75,226,61,29,19,199,63,20,52,159,21,90,187,235,214,177,172,240,90,124,23,248,69,224,150,146,38,100,253,244,98,235,193,247,40,88,112,26,38,76,101,13,92,178,217,251,76,84,185,47,237,27,233,211,158,15,242,65,237,21,163,173,172,151,228,126,207,126,197,223,180,253,159,195,223,217,251,246,60,89,181,49,110,154,39,236,227,240,123,74,111,223,51,72,4,95,7,116,123,36,93,177,72,164,42,205,21,202,241,176,129,140,238,25,53,247,111,252,60,3,72,255,0,160,244,159,149,231,110,63,231,239,252,107,231,239,248,32,159,252,17,167,197,95,181,151,193,143,132,31,180,199,237,201,225,95,16,120,99,246,95,183,248,79,225,109,51,224,79,193,27,157,79,196,254,11,241,199,199,180,127,6,105,154,69,167,198,31,22,234,122,6,165,97,171,120,31,224,165,186,27,187,159,10,91,219,92,219,106,158,51,185,158,223,196,43,46,159,224,139,93,48,252,66,250,19,193,255,0,179,63,252,19,247,198,63,240,83,63,136,31,177,22,157,251,11,120,62,79,132,94,22,253,175,62,16,252,31,240,159,199,43,63,218,43,246,192,212,127,225,100,124,46,241,239,252,19,231,246,235,248,201,241,14,207,195,109,15,199,193,166,220,124,64,240,87,237,177,251,5,124,85,240,46,181,172,218,220,95,105,176,255,0,101,107,254,9,191,240,254,149,226,223,10,234,26,180,253,117,50,111,105,55,55,36,155,126,125,211,236,200,140,212,82,87,108,103,252,55,254,145,255,0,65,233,63,242,115,255,0,146,232,255,0,134,255,0,210,63,232,61,39,254,78,127,242,93,126,225,255,0,195,135,127,224,149,31,244,108,23,255,0,248,144,63,180,247,127,251,173,20,127,195,135,127,224,149,31,244,108,23,255,0,248,144,95,180,247,255,0,62,138,205,100,49,211,223,90,91,191,151,151,145,94,215,205,255,0,95,51,240,243,254,27,255,0,72,255,0,160,244,159,249,57,255,0,201,116,127,195,127,233,31,244,30,147,255,0,39,63,249,46,191,112,255,0,225,195,191,240,74,143,250,54,11,255,0,252,72,47,218,123,255,0,159,69,31,240,225,223,248,37,71,253,27,5,255,0,254,36,23,237,61,255,0,207,162,143,236,21,252,235,167,127,47,47,32,246,190,111,250,249,159,135,159,240,223,250,71,253,7,164,255,0,201,207,254,75,163,254,27,255,0,72,255,0,160,244,159,249,57,255,0,201,117,251,135,255,0,14,29,255,0,130,84,127,209,176,95,255,0,226,65,126,211,223,252,250,40,255,0,135,14,255,0,193,42,63,232,216,47,255,0,241,32,191,105,239,254,125,20,127,96,175,231,93,59,249,121,121,7,181,243,127,215,204,252,60,255,0,134,255,0,210,63,232,61,39,254,78,127,242,93,31,240,223,250,71,253,7,164,255,0,201,207,254,75,175,220,63,248,112,239,252,18,163,254,141,130,255,0,255,0,18,11,246,158,255,0,231,209,71,252,56,119,254,9,81,255,0,70,193,127,255,0,137,5,251,79,127,243,232,163,251,5,127,58,233,223,203,203,200,61,175,155,254,190,103,225,231,252,55,254,145,255,0,65,233,63,242,115,255,0,146,232,255,0,134,255,0,210,63,232,61,39,254,78,127,242,93,126,225,255,0,195,135,127,224,149,31,244,108,23,255,0,248,144,95,180,247,255,0,62,138,63,225,195,223,240,74,159,250,54,11,255,0,252,72,47,218,123,255,0,159,69,31,216,43,249,215,78,254,94,94,65,237,124,223,245,243,63,15,63,225,224,26,63,253,7,164,231,254,191,1,252,190,215,95,52,124,118,253,184,52,173,107,226,151,236,93,168,174,180,238,190,19,253,165,188,85,226,9,24,139,191,221,37,207,236,117,251,88,248,85,100,27,174,79,62,103,137,81,120,193,253,231,92,100,31,174,127,105,207,217,123,254,9,171,240,95,246,224,213,127,100,79,6,126,198,26,111,137,173,36,208,255,0,224,158,186,142,145,227,217,62,49,254,213,250,199,133,180,47,19,124,108,255,0,130,141,124,13,253,148,255,0,107,255,0,132,222,58,241,70,151,251,84,218,46,155,241,131,68,248,3,251,98,126,199,190,52,240,150,129,14,152,110,116,203,95,138,214,222,36,241,65,187,209,53,207,12,105,250,159,233,143,197,143,248,55,191,254,9,251,226,95,30,254,204,122,207,195,223,128,122,102,135,225,31,135,255,0,28,53,255,0,23,124,117,211,53,191,143,159,180,228,186,151,140,126,21,94,254,205,159,180,39,128,180,127,13,248,65,219,226,53,233,183,241,20,95,27,252,111,240,115,87,149,214,231,71,39,73,240,182,169,31,246,140,171,35,105,58,165,210,201,85,57,54,166,182,113,251,226,151,111,235,240,19,169,117,109,122,126,12,254,117,252,89,251,79,120,243,246,147,241,191,132,62,9,124,12,240,238,173,241,59,226,159,197,61,74,63,15,120,43,193,158,29,184,181,147,84,214,53,33,101,115,117,116,30,233,175,97,181,210,116,27,61,51,78,184,191,213,47,174,166,182,178,211,116,203,107,221,75,81,185,179,211,236,238,175,160,254,160,127,224,143,223,240,77,107,15,216,167,225,4,127,20,254,48,248,55,72,135,246,214,248,205,165,93,205,241,143,196,63,240,144,88,120,214,111,135,222,22,189,215,165,214,60,59,240,83,193,154,237,142,159,21,182,143,164,91,88,219,120,122,239,197,43,167,77,168,193,171,120,174,214,118,26,246,185,160,232,190,18,58,111,233,119,194,111,217,223,246,127,248,9,253,191,255,0,10,47,224,103,193,223,130,255,0,240,149,255,0,101,127,194,83,255,0,10,155,225,151,130,190,28,255,0,194,75,253,133,253,165,253,137,253,191,255,0,8,126,137,103,253,179,246,63,237,157,95,236,191,104,243,62,207,253,171,115,228,236,243,229,221,236,85,233,97,48,52,176,137,184,251,211,122,95,252,136,148,156,143,203,207,248,40,119,236,75,241,79,246,183,95,29,175,195,175,16,120,3,68,255,0,132,159,254,9,143,255,0,5,50,253,140,52,227,227,93,83,196,90,127,149,241,83,246,203,147,246,76,147,225,110,187,122,52,47,10,234,62,95,195,251,17,240,31,197,227,95,187,143,204,212,109,63,180,180,211,167,105,58,183,155,116,45,63,80,232,162,187,20,98,156,164,149,156,173,127,59,108,72,87,192,31,14,127,229,41,191,182,71,253,152,7,252,19,79,255,0,90,43,254,10,197,95,127,215,241,7,255,0,5,252,255,0,130,213,254,212,255,0,240,71,207,248,42,106,127,195,52,120,7,246,127,241,199,252,52,71,236,1,251,45,255,0,194,107,255,0,11,211,194,191,17,124,77,253,151,255,0,10,147,246,138,253,187,255,0,225,28,255,0,132,91,254,16,15,138,190,24,251,23,159,255,0,11,55,94,251,119,218,254,221,230,125,146,211,200,251,55,151,55,218,40,15,237,242,138,255,0,48,79,248,141,91,254,10,155,255,0,68,15,246,0,255,0,195,89,251,69,127,244,85,81,255,0,17,171,127,193,83,127,232,129,254,192,31,248,107,63,104,175,254,138,170,0,255,0,79,186,43,252,193,63,226,53,111,248,42,111,253,16,63,216,3,255,0,13,103,237,21,255,0,209,85,71,252,70,173,255,0,5,77,255,0,162,7,251,0,127,225,172,253,162,191,250,42,168,3,253,62,232,175,243,4,255,0,136,213,191,224,169,191,244,64,255,0,96,15,252,53,159,180,87,255,0,69,85,31,241,26,183,252,21,55,254,136,31,236,1,255,0,134,179,246,138,255,0,232,170,160,15,244,251,162,191,204,19,254,35,86,255,0,130,166,255,0,209,3,253,128,63,240,214,126,209,95,253,21,84,127,196,106,223,240,84,223,250,32,127,176,7,254,26,207,218,43,255,0,162,170,128,63,211,238,138,255,0,48,79,248,141,91,254,10,155,255,0,68,15,246,0,255,0,195,89,251,69,127,244,85,81,255,0,17,171,127,193,83,127,232,129,254,192,31,248,107,63,104,175,254,138,170,0,254,255,0,62,35,127,202,83,127,99,127,251,48,15,248,41,103,254,180,87,252,18,118,190,255,0,175,226,15,254,8,9,255,0,5,171,253,169,255,0,224,176,127,240,84,215,255,0,134,151,240,15,236,255,0,224,127,248,103,127,216,3,246,164,255,0,132,43,254,20,95,133,126,34,248,103,251,83,254,22,223,237,21,251,8,127,194,71,255,0,9,79,252,39,255,0,21,124,79,246,223,35,254,21,150,131,246,31,178,125,135,203,251,93,223,159,246,159,50,31,179,255,0,111,148,0,81,69,20,1,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 5311; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

