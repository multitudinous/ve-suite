#ifndef GETVESUITE_REquil_REquil_ICON2_H
#define GETVESUITE_REquil_REquil_ICON2_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_REquil_REquil_ICON2( void )
{
    unsigned char osgData[ 6404 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,125,0,63,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,254,254,43,243,251,246,235,255,0,130,154,126,201,191,240,79,95,10,166,173,241,207,198,205,171,120,243,80,93,2,231,195,95,1,126,28,93,120,95,196,63,29,252,87,163,248,131,84,213,180,200,188,85,165,120,19,89,241,62,152,186,111,129,237,255,0,225,28,241,59,220,107,218,181,230,155,163,121,222,30,155,75,183,191,184,215,46,116,237,42,246,63,248,41,215,237,223,225,95,248,39,127,236,145,227,143,142,250,180,15,170,248,247,85,107,143,134,255,0,0,252,42,254,29,213,60,73,166,120,191,227,207,137,60,51,226,93,91,192,58,63,138,32,211,117,125,52,105,222,7,183,30,27,213,181,109,126,234,93,87,77,117,209,124,53,127,14,157,113,54,181,54,153,97,121,254,118,58,70,147,241,47,246,161,248,153,226,255,0,143,31,30,188,99,173,252,80,248,165,241,75,93,127,16,120,179,197,254,35,150,222,75,207,18,95,220,67,111,167,217,147,111,101,4,86,158,30,240,254,159,165,219,219,88,233,26,77,138,67,166,232,250,117,157,134,157,167,88,219,90,91,91,192,57,177,24,136,208,143,121,21,24,185,51,244,39,246,138,255,0,130,215,127,193,79,255,0,108,205,87,197,26,127,129,190,36,201,251,42,252,32,214,174,44,180,253,55,192,63,0,177,225,207,20,216,232,186,119,141,110,252,85,161,234,154,167,199,203,219,17,227,27,175,23,127,100,191,135,116,221,102,251,195,151,254,17,209,117,75,93,37,162,93,2,206,223,81,212,160,185,252,253,241,239,195,191,139,255,0,31,117,171,95,22,252,125,248,193,241,19,227,111,141,180,237,38,47,15,233,222,36,248,169,227,95,23,124,87,241,54,153,225,123,59,219,221,70,207,64,180,215,252,119,169,106,119,150,122,28,90,182,175,172,220,199,103,20,235,107,28,218,189,196,203,18,205,113,51,63,232,175,195,63,217,242,221,109,160,75,235,45,194,88,85,86,198,40,131,199,11,49,3,247,99,126,119,6,107,120,247,201,194,249,78,131,104,218,43,235,189,27,224,4,113,220,52,137,164,218,64,173,108,118,205,38,30,54,82,241,48,10,74,160,201,0,16,67,28,128,120,60,17,224,214,204,101,119,122,143,229,183,245,242,238,104,160,150,250,179,241,7,225,230,131,251,66,126,207,39,91,79,217,215,246,128,248,189,240,50,95,21,54,152,60,105,15,194,31,137,222,61,248,71,47,137,151,67,93,65,188,58,222,34,95,0,235,186,88,215,197,144,214,117,175,178,125,179,206,54,191,219,55,38,216,70,46,103,47,250,133,251,33,255,0,193,124,255,0,111,207,217,79,198,126,7,240,215,237,89,173,203,251,85,126,207,208,195,224,239,14,120,154,31,19,104,62,29,181,248,237,225,63,7,248,119,75,213,52,75,157,107,192,159,19,116,104,180,117,241,199,142,229,55,250,5,254,164,60,124,117,155,159,17,73,225,200,237,255,0,225,35,208,46,181,93,67,93,30,217,175,254,207,176,72,117,6,151,71,183,149,30,220,163,77,16,1,8,123,101,139,8,165,88,22,249,130,133,223,203,28,113,158,62,48,248,171,251,60,219,221,219,93,37,189,138,79,105,42,121,79,101,58,8,98,72,164,66,174,195,36,180,82,121,45,20,156,130,140,214,236,251,70,21,169,209,204,166,164,151,59,105,116,98,112,93,52,63,186,111,216,247,246,224,253,155,63,110,159,134,90,119,196,207,217,227,226,46,143,226,95,248,147,104,154,151,141,126,30,94,106,58,69,183,197,111,132,122,134,187,54,179,103,105,225,175,139,62,4,179,213,46,110,124,27,172,54,163,225,191,17,195,105,51,180,186,102,179,22,135,54,165,225,253,71,87,209,228,182,212,174,62,181,175,242,200,248,123,241,79,227,175,252,19,219,227,207,134,63,105,143,217,163,196,167,195,62,54,240,209,154,195,81,210,181,4,150,235,193,254,60,240,158,165,113,103,115,173,252,42,248,137,225,196,189,183,30,39,248,127,170,77,167,219,180,145,45,220,119,86,23,80,233,250,206,145,121,166,106,250,126,159,169,91,127,165,159,236,167,251,78,124,45,253,178,191,103,207,134,95,180,191,193,139,157,118,127,135,95,20,244,123,221,75,71,183,241,78,137,63,135,124,77,163,106,90,30,185,170,248,83,197,158,23,241,22,145,52,146,37,190,187,164,120,195,65,215,180,187,199,180,184,188,211,167,184,209,228,184,211,47,245,13,58,91,91,235,143,127,15,93,87,135,50,209,163,54,172,236,127,7,63,240,91,31,218,34,255,0,246,206,255,0,130,158,252,68,240,61,143,136,231,215,62,18,254,202,23,47,240,27,225,253,141,149,151,140,236,116,77,59,197,30,29,138,194,127,143,58,165,254,129,226,171,131,107,23,141,37,248,194,222,38,240,222,161,174,105,214,22,182,186,182,135,240,171,195,241,195,253,161,111,101,14,165,123,163,251,62,124,50,183,16,216,223,164,41,42,176,72,108,226,68,220,98,73,121,85,36,35,52,110,10,151,111,41,17,84,49,10,112,138,163,242,119,194,127,181,79,195,79,139,223,24,126,47,252,123,248,149,241,83,224,231,132,124,113,241,171,226,55,142,62,42,248,155,74,180,241,198,129,160,248,95,79,241,63,197,63,22,234,190,55,241,29,150,135,101,175,248,134,230,242,211,66,183,214,60,75,168,199,107,21,197,229,221,196,86,214,209,164,247,23,18,172,146,201,251,25,240,55,246,179,253,145,180,120,244,168,245,127,218,191,246,99,176,22,214,228,206,183,255,0,29,126,25,90,130,31,97,242,177,117,226,69,203,226,22,5,88,114,92,114,115,199,135,152,58,242,156,229,26,114,151,77,19,127,161,164,108,151,102,207,212,111,133,223,11,146,88,226,30,88,251,94,16,92,92,42,1,34,201,130,81,85,55,49,3,115,135,37,242,84,149,85,80,193,84,125,153,225,207,131,107,112,169,52,58,122,62,235,98,60,201,148,229,128,120,182,239,140,54,229,144,168,70,229,143,7,56,25,227,227,191,133,63,183,95,252,19,246,209,108,98,213,255,0,110,63,216,202,196,0,178,76,111,127,105,191,131,22,136,28,28,43,48,155,198,74,60,208,198,66,119,14,140,185,206,6,62,238,240,167,252,20,63,254,9,145,36,42,47,191,224,161,223,176,181,170,131,132,130,231,246,184,253,159,173,151,110,214,77,210,111,248,130,48,73,49,245,81,141,184,0,40,38,190,110,181,44,85,221,168,77,255,0,219,178,255,0,35,120,242,90,238,73,223,211,250,234,112,190,42,248,60,182,203,116,205,98,32,150,65,26,164,241,35,49,86,49,38,198,84,3,32,6,242,195,28,176,249,176,87,230,24,248,119,226,175,195,36,137,46,140,86,235,29,226,198,231,33,88,249,208,0,206,202,74,225,134,21,202,101,3,71,135,12,160,4,202,254,132,120,199,254,10,25,255,0,4,205,49,201,21,151,252,20,47,246,26,188,138,97,34,50,195,251,90,126,207,247,34,38,125,197,10,20,248,128,229,163,219,229,231,42,126,239,32,48,192,248,3,226,183,237,211,251,2,221,165,202,233,127,182,223,236,113,168,44,50,147,26,217,126,210,191,7,47,67,192,75,49,142,1,31,140,155,32,254,244,16,49,145,32,25,35,24,116,105,226,183,246,21,18,127,221,127,228,41,40,89,180,213,253,127,174,231,227,167,237,21,240,186,202,72,111,101,104,96,142,219,80,18,71,44,44,138,133,174,37,50,67,20,142,129,25,11,199,187,99,134,132,96,28,130,236,138,107,237,15,248,54,175,246,150,212,126,7,254,217,159,19,191,98,223,17,120,146,234,211,225,247,237,23,225,45,123,198,222,1,240,246,171,103,227,75,212,147,227,215,194,187,43,93,98,99,225,107,59,11,169,52,111,7,79,173,252,22,131,226,13,215,136,175,239,108,225,125,96,252,28,240,229,164,122,148,83,217,219,105,250,135,203,63,31,63,106,111,217,79,91,178,214,96,209,255,0,106,175,217,171,83,121,30,71,139,236,31,28,62,28,94,172,161,211,98,136,205,175,136,92,187,101,57,3,179,14,185,227,243,15,193,63,183,222,131,251,24,254,212,191,15,63,107,79,128,190,59,248,23,227,223,136,191,12,71,138,46,180,29,27,198,30,39,179,241,87,130,110,231,241,175,129,188,81,240,211,87,143,86,211,188,25,227,29,46,254,117,143,195,254,58,214,231,183,16,234,86,236,151,86,150,242,74,101,130,57,173,230,250,108,189,214,132,227,205,9,70,45,235,116,253,63,3,9,217,171,223,85,253,126,167,206,255,0,178,247,252,122,254,200,71,254,167,141,3,158,159,243,108,127,22,123,142,107,246,1,191,228,220,63,224,175,156,255,0,204,165,241,103,211,254,145,217,240,67,219,165,126,63,254,203,191,241,231,251,32,255,0,216,239,225,255,0,253,102,47,139,53,251,0,127,228,220,63,224,175,159,246,41,124,89,255,0,215,118,124,16,171,196,127,27,209,71,255,0,78,196,168,236,255,0,174,136,253,237,241,191,252,151,79,248,38,167,253,158,39,141,61,127,233,31,31,183,173,111,124,85,255,0,147,50,255,0,131,148,191,236,19,251,68,115,129,159,249,66,119,236,103,212,227,208,214,15,141,255,0,228,186,255,0,193,52,255,0,236,241,60,105,255,0,174,248,253,189,43,123,226,175,252,153,151,252,28,167,255,0,96,159,218,35,255,0,92,157,251,25,215,207,211,111,218,80,215,120,67,191,253,5,71,243,53,91,63,235,236,163,235,239,218,36,255,0,198,200,255,0,101,31,251,50,15,248,40,135,254,175,159,248,38,143,229,95,132,191,25,127,229,23,63,240,80,78,58,248,179,254,11,43,249,127,195,84,126,216,92,103,211,53,251,179,251,68,255,0,202,72,255,0,101,47,251,50,15,248,40,135,254,175,159,248,38,141,126,19,124,101,255,0,148,92,127,193,64,255,0,236,108,255,0,130,203,127,235,84,126,216,84,240,141,251,60,39,248,169,127,233,202,194,147,247,165,253,116,129,242,31,237,107,255,0,39,49,240,207,211,254,20,15,237,69,239,255,0,53,7,246,68,61,255,0,207,21,252,214,126,212,223,242,74,190,35,255,0,217,103,248,175,255,0,173,21,241,14,191,165,63,218,215,254,78,99,225,159,253,144,31,218,139,255,0,86,15,236,135,95,205,103,237,79,255,0,36,171,226,55,253,150,127,138,255,0,250,209,95,16,235,223,192,95,217,225,219,123,242,255,0,233,82,49,151,218,254,191,148,250,131,246,93,255,0,143,63,217,7,254,199,127,15,255,0,235,49,124,89,175,216,3,255,0,38,225,255,0,5,124,255,0,177,75,226,207,254,187,179,224,133,126,63,254,203,191,241,231,251,32,255,0,216,239,225,255,0,253,102,47,139,53,251,0,127,228,220,63,224,175,159,246,41,124,89,255,0,215,118,124,16,163,17,252,103,232,191,244,244,74,142,223,119,228,143,222,223,27,255,0,201,117,255,0,130,105,255,0,217,226,120,211,255,0,93,241,251,122,86,247,197,95,249,51,47,248,57,79,254,193,63,180,71,254,185,59,246,51,172,31,27,255,0,201,117,255,0,130,105,255,0,217,226,120,211,255,0,93,241,251,122,86,247,197,95,249,51,47,248,57,79,254,193,63,180,71,254,185,59,246,51,175,158,167,252,74,31,224,135,254,165,68,213,109,47,95,253,177,31,94,254,209,63,242,146,63,217,75,254,204,131,254,10,33,255,0,171,231,254,9,163,95,132,223,25,127,229,23,31,240,80,63,251,27,63,224,178,223,250,213,31,182,21,126,236,254,209,63,242,146,63,217,75,254,204,131,254,10,33,255,0,171,231,254,9,163,95,132,223,25,127,229,23,31,240,80,63,251,27,63,224,178,223,250,213,31,182,21,86,23,224,194,127,138,151,254,156,172,41,124,82,249,255,0,237,135,200,127,181,175,252,156,199,195,63,251,32,63,181,23,254,172,31,217,14,191,154,207,218,159,254,73,87,196,111,251,44,255,0,21,255,0,245,162,190,33,215,244,167,251,90,255,0,201,204,124,51,255,0,178,3,251,81,127,234,193,253,144,235,249,172,253,169,255,0,228,149,124,70,255,0,178,207,241,95,255,0,90,43,226,29,125,6,3,248,88,111,251,119,255,0,74,145,140,190,215,245,252,167,212,31,178,239,252,121,254,200,63,246,59,248,127,255,0,89,139,226,205,126,192,31,249,55,15,248,43,231,253,138,95,22,127,245,221,159,4,43,241,255,0,246,93,255,0,143,63,217,7,254,199,127,15,255,0,235,49,124,89,175,216,3,255,0,38,225,255,0,5,124,255,0,177,75,226,207,254,187,179,224,133,44,71,241,159,162,255,0,211,209,42,59,125,223,146,63,123,124,111,255,0,37,215,254,9,167,255,0,103,137,227,79,253,119,199,237,233,91,223,21,127,228,204,191,224,229,63,251,4,254,209,31,250,228,239,216,206,176,124,111,255,0,37,215,254,9,167,255,0,103,137,227,79,253,119,199,237,233,91,223,21,127,228,204,191,224,229,63,251,4,254,209,31,250,228,239,216,206,190,122,159,241,40,127,130,31,250,149,19,85,180,189,127,246,196,125,123,251,68,255,0,202,72,255,0,101,47,251,50,15,248,40,135,254,175,159,248,38,141,126,19,124,101,255,0,148,92,127,193,64,255,0,236,108,255,0,130,203,127,235,84,126,216,85,251,179,251,68,255,0,202,72,255,0,101,47,251,50,15,248,40,135,254,175,159,248,38,141,126,19,124,101,255,0,148,92,127,193,64,255,0,236,108,255,0,130,203,127,235,84,126,216,85,88,95,131,9,254,42,95,250,114,176,165,241,75,231,255,0,182,31,33,254,214,191,242,115,31,12,255,0,236,128,254,212,95,250,176,127,100,58,254,107,63,106,127,249,37,95,17,191,236,179,252,87,255,0,214,138,248,135,95,210,159,237,107,255,0,39,49,240,207,254,200,15,237,69,255,0,171,7,246,67,175,230,179,246,167,255,0,146,85,241,27,254,203,63,197,127,253,104,175,136,117,244,24,15,225,97,191,237,223,253,42,70,50,251,95,215,242,159,80,126,203,191,241,231,251,32,255,0,216,239,225,255,0,253,102,47,139,53,251,0,127,228,220,63,224,175,159,246,41,124,89,255,0,215,118,124,16,175,198,79,217,174,79,136,54,246,159,1,134,159,240,111,199,186,248,248,101,226,45,19,88,215,155,74,215,62,17,65,246,203,88,254,13,248,207,192,1,52,133,215,126,40,217,25,231,109,103,197,122,100,170,39,22,235,246,84,153,217,150,100,88,31,245,239,66,178,253,160,188,79,240,151,246,235,240,38,135,251,35,124,109,190,213,255,0,106,63,15,248,218,203,225,245,207,252,38,191,178,165,182,155,164,191,137,255,0,101,63,135,127,3,44,37,241,124,215,127,180,156,114,233,240,167,139,60,29,170,92,205,246,56,117,6,93,58,104,38,69,123,151,123,72,214,35,151,218,185,57,197,45,55,148,87,252,189,140,186,190,218,142,46,203,254,3,236,143,232,55,198,255,0,242,93,127,224,154,127,246,120,158,52,255,0,215,124,126,222,149,189,241,87,254,76,203,254,14,83,255,0,176,79,237,17,255,0,174,78,253,140,235,230,171,159,136,191,180,95,142,62,37,254,199,254,47,240,247,252,19,243,246,171,184,211,190,3,252,121,241,7,197,95,22,219,207,227,239,216,54,214,255,0,84,240,238,173,251,46,126,211,95,4,45,173,60,51,20,223,182,168,142,239,89,95,22,124,101,240,197,195,197,119,45,148,35,79,176,191,153,110,26,230,43,123,75,175,89,241,37,183,237,139,227,175,217,243,254,10,229,240,179,65,255,0,130,111,126,213,79,226,15,219,238,199,226,173,175,193,219,219,191,138,223,240,78,219,109,27,195,82,120,231,254,9,227,240,3,246,73,210,79,196,201,207,237,216,110,52,69,139,226,63,194,159,17,222,220,255,0,102,91,107,5,52,75,187,59,152,196,183,207,113,166,219,120,48,130,140,232,185,85,166,148,99,20,255,0,121,14,152,136,205,253,175,229,87,53,79,71,163,215,201,255,0,42,93,187,159,124,254,209,63,242,146,63,217,75,254,204,131,254,10,33,255,0,171,231,254,9,163,95,132,223,25,127,229,23,31,240,80,63,251,27,63,224,178,223,250,213,31,182,21,126,168,124,103,248,159,251,80,248,143,246,183,248,37,241,226,15,248,38,167,237,133,167,248,71,225,199,236,235,251,85,124,31,214,244,205,71,226,103,252,19,143,254,18,107,207,19,124,113,248,151,251,33,120,207,194,119,186,77,150,159,251,124,220,91,79,161,219,105,127,0,124,101,30,165,44,215,144,92,65,113,168,233,169,111,109,119,28,183,114,217,126,69,252,90,210,127,106,155,143,216,203,246,168,253,157,238,63,97,31,218,43,77,241,175,198,93,115,246,253,212,188,53,170,94,252,69,253,136,228,240,190,155,7,237,87,241,167,227,223,196,127,135,139,226,27,205,63,246,189,158,242,218,107,61,19,226,126,131,22,178,45,108,111,5,189,221,149,228,118,77,168,197,28,19,220,60,52,82,142,25,58,180,239,9,83,111,247,148,246,140,234,183,246,187,73,125,226,110,238,78,206,218,244,127,221,242,242,103,204,255,0,181,175,252,156,199,195,63,251,32,63,181,23,254,172,31,217,14,191,154,207,218,159,254,73,87,196,111,251,44,255,0,21,255,0,245,162,190,33,215,244,37,251,73,107,95,27,124,67,241,127,194,255,0,16,91,246,73,248,231,162,232,190,21,248,95,241,151,193,154,165,190,175,226,255,0,217,85,245,51,170,124,68,241,79,192,205,107,68,184,181,131,70,253,165,47,34,155,79,138,215,225,119,136,86,233,204,203,44,114,93,89,136,161,157,100,153,237,255,0,159,223,218,87,194,255,0,18,175,60,25,226,223,13,95,252,45,241,78,131,127,174,248,247,198,158,48,180,147,84,215,62,26,79,107,30,151,226,127,139,62,37,241,182,157,21,196,154,31,143,239,25,111,87,75,241,21,132,83,34,35,162,220,172,138,178,60,74,38,111,127,2,173,10,17,230,139,148,121,111,105,69,253,169,118,126,104,202,93,95,127,248,7,235,223,137,254,22,159,217,71,246,196,253,164,191,102,40,173,252,101,165,105,63,8,190,52,124,71,240,39,130,7,196,109,37,236,252,107,172,120,7,65,241,85,227,124,41,241,78,176,241,104,58,117,190,161,253,177,240,213,188,25,172,91,106,22,182,54,218,126,169,105,226,40,181,11,40,254,201,115,6,223,214,239,217,247,196,250,100,118,186,68,193,221,190,207,178,22,95,151,112,105,124,163,19,180,106,197,210,35,181,121,42,8,14,167,111,53,237,159,240,114,71,236,11,226,15,135,223,20,116,191,248,41,127,194,191,50,111,11,248,234,79,0,124,47,253,163,124,63,97,97,227,13,71,82,209,124,117,165,233,119,126,26,240,7,198,13,79,85,154,234,251,73,208,252,11,170,248,79,67,240,119,130,111,162,104,116,91,107,77,115,73,240,191,217,211,89,212,252,85,123,37,143,228,199,236,239,241,138,41,96,177,188,51,63,217,46,148,11,155,118,104,220,65,117,33,49,249,102,52,46,155,240,36,37,84,196,229,247,140,3,36,107,92,249,142,30,74,78,223,11,215,242,254,191,164,56,187,171,118,63,167,47,132,158,48,183,183,91,88,227,100,13,108,232,34,108,228,60,100,198,174,119,28,237,114,202,48,25,7,59,144,101,134,107,239,223,9,124,75,182,16,134,130,240,71,189,124,199,138,115,181,24,133,85,220,1,206,78,249,72,32,12,110,66,120,57,90,254,127,190,27,252,96,183,138,11,104,141,231,153,108,130,53,134,233,36,18,205,26,177,92,239,27,142,196,217,36,50,101,183,40,32,242,140,64,31,95,104,159,25,64,101,71,186,134,119,91,98,207,251,241,28,219,139,161,38,71,5,65,0,182,48,29,135,35,239,99,112,249,138,216,119,41,59,236,254,102,209,157,146,77,31,165,254,53,248,149,110,241,77,190,239,206,148,171,71,26,33,13,20,109,40,5,66,141,132,12,249,167,157,167,11,25,0,30,181,249,225,241,115,197,214,142,147,44,133,25,134,235,169,29,88,99,122,128,193,2,174,226,229,188,180,3,8,63,141,128,43,211,144,241,23,198,84,101,188,79,183,69,110,22,48,205,178,85,146,112,158,64,18,6,118,200,144,121,108,205,131,33,198,209,220,109,63,22,252,80,248,191,109,53,181,210,201,112,33,180,8,100,35,121,19,206,224,6,10,246,225,195,50,229,224,140,12,128,67,29,177,128,65,87,67,14,238,157,172,180,9,78,250,45,17,243,175,237,15,226,59,6,179,188,251,60,204,179,220,76,215,22,191,44,69,147,229,142,56,94,72,139,18,138,100,84,198,84,168,243,23,118,209,146,62,69,255,0,130,119,126,207,31,15,255,0,110,15,248,41,223,193,159,128,255,0,23,126,29,107,95,22,190,5,92,233,95,21,117,191,139,126,30,210,27,199,90,30,159,164,248,91,194,191,9,60,103,113,225,253,123,196,222,45,240,37,246,159,168,248,79,69,79,139,83,252,44,182,75,195,168,90,67,115,169,235,118,26,83,73,40,212,62,201,113,231,127,180,119,198,35,28,119,215,34,87,145,174,26,72,52,219,97,44,68,17,116,174,141,42,238,2,50,21,247,13,201,28,167,110,246,201,15,27,87,244,185,255,0,6,218,127,193,62,117,223,132,223,14,124,79,255,0,5,1,248,178,92,120,255,0,246,153,240,108,94,19,248,53,160,94,233,254,50,209,117,223,7,252,11,131,197,115,106,218,223,137,181,232,53,107,203,109,63,90,255,0,132,247,196,30,26,240,78,181,163,203,14,153,117,20,62,23,240,150,129,170,233,122,228,177,120,159,81,211,236,254,171,46,195,190,117,46,145,213,255,0,95,129,140,218,181,186,159,211,159,137,124,53,225,207,26,120,115,196,30,14,241,143,135,244,79,22,120,71,197,154,38,171,225,175,20,248,91,196,186,85,134,187,225,207,18,248,115,93,176,159,75,215,60,63,226,13,15,84,130,91,93,103,68,188,211,46,174,173,174,237,46,98,146,11,136,46,100,134,104,222,55,101,63,231,147,255,0,5,21,255,0,130,69,254,210,63,240,75,73,245,255,0,140,62,14,241,15,252,46,63,216,238,239,198,175,164,104,63,16,98,121,91,226,7,195,141,39,86,26,55,252,34,58,111,237,27,165,219,232,22,214,58,84,215,26,213,228,250,21,151,137,116,185,39,209,117,139,205,38,15,237,11,127,10,234,222,34,209,52,27,159,244,81,162,189,186,180,163,86,60,178,90,25,166,214,204,255,0,49,31,132,159,180,148,30,80,91,125,64,90,72,90,222,9,44,238,95,202,242,102,151,136,154,2,210,102,115,182,89,162,27,137,67,228,103,114,0,160,125,129,163,126,209,104,89,174,101,66,144,188,111,26,27,114,229,217,188,200,242,194,75,76,169,139,42,224,141,217,220,131,43,144,13,126,253,254,214,31,240,109,23,236,81,241,183,86,241,63,141,255,0,103,175,20,248,235,246,62,241,238,187,37,189,253,174,139,224,139,109,55,199,127,0,172,245,235,175,26,223,120,159,196,186,233,248,59,226,41,109,175,244,49,119,163,234,215,250,101,158,149,225,111,21,248,91,195,218,58,88,105,115,88,104,201,29,157,205,174,161,252,145,255,0,193,85,63,100,175,22,255,0,193,41,255,0,104,79,7,126,207,3,227,109,199,199,63,248,76,190,12,248,123,227,63,252,37,182,254,8,151,225,106,105,199,196,62,55,248,137,224,127,248,71,79,135,238,124,109,226,118,188,120,127,225,93,253,171,237,131,80,132,73,253,174,32,22,113,155,115,53,199,143,87,45,149,220,149,154,118,253,11,83,238,181,62,213,215,63,104,172,60,210,71,181,99,184,95,42,41,230,56,146,41,124,149,69,103,123,182,82,239,184,51,40,87,99,136,255,0,135,138,248,139,227,23,237,45,11,44,242,11,232,239,110,100,181,146,120,32,89,119,216,196,169,201,158,107,134,96,98,143,207,105,100,109,224,46,45,8,17,182,21,171,219,127,224,145,255,0,240,78,47,22,255,0,193,93,127,225,160,115,251,72,75,240,11,254,25,243,254,21,79,252,124,252,47,184,248,179,255,0,9,103,252,45,143,248,89,89,217,246,95,137,222,20,254,192,251,7,252,43,94,119,125,191,237,95,219,35,31,101,251,49,251,79,245,179,251,15,127,193,191,63,177,31,236,113,227,47,3,252,97,215,166,241,223,237,39,241,211,192,191,240,134,235,222,29,241,159,197,203,237,46,219,192,254,9,248,145,225,189,43,86,182,213,60,115,240,219,225,55,132,244,235,61,63,77,150,231,89,213,255,0,180,52,212,241,53,199,139,245,15,14,92,248,127,71,186,209,181,123,125,82,198,77,82,233,208,203,118,148,173,20,193,212,236,172,126,13,127,193,40,63,224,138,63,26,255,0,107,175,24,252,15,253,178,191,106,193,163,120,67,246,84,179,215,45,62,35,248,87,225,111,137,52,171,125,111,226,15,237,13,165,248,114,247,195,218,183,130,45,245,127,7,248,155,195,55,26,78,153,251,55,248,142,101,187,123,235,189,77,230,212,124,81,162,232,211,65,165,104,144,104,30,39,210,188,92,191,221,181,20,87,175,78,156,105,69,70,42,201,25,159,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 6404; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

