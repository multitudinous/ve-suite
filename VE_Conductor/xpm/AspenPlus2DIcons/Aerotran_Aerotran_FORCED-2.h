#ifndef GETVESUITE_Aerotran_Aerotran_FORCED-2_H
#define GETVESUITE_Aerotran_Aerotran_FORCED-2_H
//Usage of this file
//std::istringstream tempStreamI( GetVESuite_whatever() );
//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "ive" )->readNode( tempStreamI ).getNode();
//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->
//    getReaderWriterForExtension( "png" )->readNode( tempStreamI ).getImage();

#include <string>

std::string GetVESuite_Aerotran_Aerotran_FORCED-2( void )
{
    unsigned char osgData[ 8230 ] = { 
        255,216,255,224,0,16,74,70,73,70,0,1,1,1,0,72,0,72,0,0,255,219,0,67,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,255,219,0,67,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,255,192,0,17,8,0,80,0,150,3,1,34,0,2,17,1,3,17,1,255,196,0,31,0,0,1,5,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,16,0,2,1,3,3,2,4,3,5,5,4,4,0,0,1,125,1,2,3,0,4,17,5,18,33,49,65,6,19,81,97,7,34,113,20,50,129,145,161,8,35,66,177,193,21,82,209,240,36,51,98,114,130,9,10,22,23,24,25,26,37,38,39,40,41,42,52,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,225,226,227,228,229,230,231,232,233,234,241,242,243,244,245,246,247,248,249,250,255,196,0,31,1,0,3,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,2,3,4,5,6,7,8,9,10,11,255,196,0,181,17,0,2,1,2,4,4,3,4,7,5,4,4,0,1,2,119,0,1,2,3,17,4,5,33,49,6,18,65,81,7,97,113,19,34,50,129,8,20,66,145,161,177,193,9,35,51,82,240,21,98,114,209,10,22,36,52,225,37,241,23,24,25,26,38,39,40,41,42,53,54,55,56,57,58,67,68,69,70,71,72,73,74,83,84,85,86,87,88,89,90,99,100,101,102,103,104,105,106,115,116,117,118,119,120,121,122,130,131,132,133,134,135,136,137,138,146,147,148,149,150,151,152,153,154,162,163,164,165,166,167,168,169,170,178,179,180,181,182,183,184,185,186,194,195,196,197,198,199,200,201,202,210,211,212,213,214,215,216,217,218,226,227,228,229,230,231,232,233,234,242,243,244,245,246,247,248,249,250,255,218,0,12,3,1,0,2,17,3,17,0,63,0,253,35,248,161,255,0,4,144,253,159,188,75,255,0,5,20,253,181,190,30,220,120,134,214,77,6,111,128,154,15,237,251,246,255,0,20,126,199,63,240,75,15,140,62,56,63,24,255,0,107,207,218,135,246,224,214,254,37,248,107,81,248,157,241,247,254,9,247,226,207,18,248,143,225,149,141,239,195,77,17,60,59,103,170,234,215,250,166,149,105,115,54,158,218,205,206,159,14,155,109,97,250,83,255,0,4,3,240,133,231,135,63,224,158,54,26,221,239,139,53,61,125,252,109,251,73,254,214,211,218,248,118,31,9,252,38,248,107,240,231,225,190,149,240,183,246,133,241,247,236,233,162,120,75,224,247,194,79,129,159,14,60,41,225,79,133,222,16,189,209,254,10,88,248,163,89,176,210,52,91,104,181,79,29,124,69,241,135,138,110,73,189,241,13,202,167,241,181,224,95,248,56,51,254,10,27,241,83,227,111,142,127,105,155,143,10,254,198,58,47,141,254,53,126,205,255,0,5,255,0,103,143,16,232,208,124,22,248,223,123,225,109,55,192,190,7,241,103,198,223,136,30,31,212,116,91,87,253,170,162,187,139,197,135,87,248,249,227,8,111,110,39,189,158,206,123,109,59,76,88,52,251,73,34,186,150,247,232,47,216,219,254,11,59,255,0,5,16,253,148,188,7,240,247,246,84,248,102,191,177,134,171,224,233,63,225,166,126,50,217,120,143,199,95,179,207,199,29,103,196,214,250,183,142,63,104,70,248,165,226,173,30,232,248,127,246,194,210,109,110,244,227,226,191,143,154,226,105,236,150,240,201,105,167,104,54,182,247,45,168,92,188,215,199,138,53,28,49,53,229,58,171,216,180,185,86,183,86,248,155,211,191,153,77,166,149,150,170,247,252,15,244,79,162,191,141,253,19,254,11,83,255,0,5,75,214,38,49,127,109,254,192,22,224,73,109,25,97,251,36,126,209,83,127,199,195,186,22,43,255,0,13,252,191,116,33,60,19,156,227,138,246,45,19,254,10,161,255,0,5,76,214,0,99,227,159,216,2,220,24,164,151,3,246,52,253,162,166,35,203,152,69,183,63,240,240,245,201,59,179,237,211,7,173,57,99,240,145,222,178,251,159,249,7,44,159,67,250,190,162,191,154,29,23,246,249,255,0,130,166,106,236,20,252,87,253,128,109,179,44,177,100,254,196,31,180,76,191,234,225,89,67,96,255,0,193,72,87,131,187,21,234,58,63,237,97,255,0,5,76,213,130,31,248,94,63,176,12,30,96,182,199,252,96,119,237,21,49,31,104,206,56,31,240,82,245,228,109,60,112,106,63,181,48,11,124,66,251,165,254,65,201,46,199,244,27,69,126,30,233,63,27,63,224,169,154,162,130,63,104,143,216,6,2,82,71,35,254,29,255,0,251,68,202,48,146,136,128,207,252,60,245,121,37,129,174,239,78,241,191,252,21,47,80,242,241,251,77,254,192,48,153,12,0,3,255,0,4,242,253,162,100,199,157,211,63,241,180,117,206,42,30,111,151,45,241,41,124,165,229,229,230,63,103,63,229,63,97,235,230,173,71,246,191,253,157,180,159,3,104,63,18,117,15,136,127,103,240,87,137,255,0,105,89,191,100,13,15,90,255,0,132,75,199,50,253,187,246,137,183,253,162,181,175,217,62,111,135,159,217,208,120,101,174,237,177,241,255,0,195,218,198,129,253,175,52,17,232,71,236,127,218,163,83,58,36,145,106,79,242,86,154,63,224,169,122,130,6,255,0,134,176,253,128,97,37,29,241,255,0,14,229,253,162,164,225,36,9,255,0,73,81,95,83,95,33,120,135,254,9,207,251,117,106,223,9,188,33,240,138,239,246,221,253,147,23,195,94,14,253,184,110,63,111,125,50,250,223,254,9,239,241,130,61,110,227,227,21,231,237,153,226,143,219,161,252,55,125,117,47,252,20,153,237,228,248,106,62,45,120,167,81,211,133,154,91,69,170,15,14,195,13,161,214,78,166,178,106,210,203,206,114,222,152,168,253,210,242,242,236,254,240,246,117,27,183,41,253,3,209,95,147,186,150,171,255,0,5,76,211,193,63,240,212,255,0,176,20,216,68,127,249,71,87,237,19,31,222,147,203,35,31,240,244,246,239,159,127,106,225,111,190,36,255,0,193,82,236,164,9,255,0,13,43,251,1,74,55,202,187,135,252,19,219,246,138,79,245,108,163,129,255,0,15,67,57,206,238,190,212,214,113,150,181,117,138,86,126,82,242,242,243,7,78,106,215,142,231,236,229,21,248,67,170,254,208,223,240,84,189,49,89,135,237,1,251,0,206,22,52,147,254,76,19,246,137,143,135,149,163,199,252,164,225,186,0,15,244,199,39,204,117,143,219,51,254,10,151,164,249,223,241,121,255,0,96,43,143,43,237,63,243,98,191,180,76,91,190,206,63,237,37,79,140,154,181,154,224,30,216,132,239,229,47,242,19,132,150,141,31,209,101,21,252,191,107,63,240,81,159,248,42,102,146,238,159,240,179,63,96,25,246,60,113,228,254,196,255,0,180,76,71,50,67,230,231,104,255,0,130,141,183,3,167,169,235,94,55,173,127,193,92,191,224,169,186,60,111,33,241,111,236,1,112,18,56,228,219,255,0,12,121,251,69,67,254,178,111,39,239,127,195,194,91,129,215,167,61,56,235,87,28,199,7,39,101,93,95,209,255,0,144,114,75,177,253,114,209,95,197,198,185,255,0,5,203,255,0,130,166,232,222,102,47,191,96,11,157,151,143,107,143,248,101,15,218,42,34,118,9,190,124,255,0,195,124,54,15,238,186,99,141,221,120,175,147,244,239,248,57,179,254,10,153,168,120,7,193,254,55,255,0,132,91,246,0,132,248,175,193,158,21,241,119,246,95,252,51,175,237,21,32,176,255,0,132,154,198,206,247,251,63,237,195,246,216,31,105,242,62,212,87,206,242,99,243,60,188,249,81,147,180,108,177,52,36,174,170,38,190,127,228,43,59,218,218,159,223,189,21,254,107,255,0,21,191,224,240,79,248,42,111,195,43,219,27,65,240,131,246,0,215,62,219,117,172,91,9,63,225,76,254,209,90,103,150,52,153,108,226,243,54,31,218,238,227,127,152,110,243,140,141,158,95,86,207,5,108,154,146,77,59,166,35,240,107,246,110,3,254,17,255,0,11,14,50,116,111,2,12,113,255,0,62,67,208,243,255,0,215,175,191,188,22,115,241,159,225,175,93,223,240,167,62,57,140,17,140,227,226,7,192,108,145,187,175,35,223,242,205,127,103,30,36,255,0,130,112,255,0,193,60,109,255,0,224,164,95,181,183,129,96,253,131,63,99,24,60,19,225,191,248,38,103,236,167,227,223,15,120,58,31,217,119,224,124,94,21,208,124,115,226,63,143,31,240,80,13,27,196,30,51,209,60,60,190,5,251,30,151,226,219,237,39,193,158,16,181,188,212,96,133,47,46,173,188,41,166,193,60,207,21,133,170,197,249,223,240,239,254,9,195,251,28,124,89,255,0,130,26,120,55,83,176,253,153,255,0,102,63,6,124,113,248,157,255,0,5,14,208,254,7,88,126,209,16,126,206,31,10,53,143,138,62,19,208,252,109,255,0,5,226,183,253,159,108,132,62,36,254,194,180,213,53,93,14,195,225,158,169,22,132,186,79,246,165,181,180,254,31,180,26,6,248,116,215,242,151,204,154,140,234,226,169,69,74,85,41,66,246,211,91,171,164,155,105,95,166,186,126,39,78,22,149,58,184,140,45,42,213,214,30,133,90,144,140,234,52,218,132,101,36,165,54,146,114,106,41,182,212,83,122,89,38,236,143,135,124,18,8,189,127,107,189,56,19,208,255,0,174,152,116,237,215,215,30,245,246,7,130,70,21,115,255,0,62,183,7,7,211,237,137,128,8,29,48,107,242,127,225,95,236,227,240,151,193,158,38,248,143,240,79,226,127,236,245,240,130,211,226,15,193,95,136,90,255,0,195,159,18,105,218,207,128,126,29,248,131,83,181,155,73,212,46,45,161,183,185,213,161,209,174,147,196,144,105,122,221,175,136,124,44,218,194,94,223,197,174,93,252,63,188,212,223,80,158,105,230,72,254,239,240,87,236,193,251,40,223,11,105,46,255,0,102,15,217,214,228,178,160,145,46,126,11,124,52,157,65,42,204,20,188,190,20,108,190,85,227,233,187,112,12,126,108,87,227,56,223,17,242,188,46,54,182,93,136,192,87,167,90,131,229,146,180,116,107,254,222,63,191,176,31,64,190,40,205,184,103,44,226,172,179,140,240,56,204,179,51,164,170,211,112,141,71,120,201,38,157,210,107,85,102,175,209,159,165,254,13,35,122,12,244,186,184,231,191,252,122,41,224,99,174,6,71,226,121,175,169,60,28,15,238,58,140,29,55,167,63,222,25,199,28,243,255,0,215,175,205,63,11,254,198,63,177,109,193,129,174,63,100,79,217,130,233,56,4,205,251,63,252,36,156,58,1,189,74,153,60,36,65,204,68,141,221,75,96,156,156,48,250,23,195,191,176,135,236,37,48,2,227,246,42,253,146,174,54,144,204,101,253,155,190,13,74,25,85,148,56,30,103,131,14,73,73,35,198,71,80,72,32,228,157,41,241,174,79,94,49,181,42,208,230,254,236,124,191,190,126,105,157,253,20,56,151,36,148,149,92,243,15,85,197,116,140,237,231,246,79,211,239,11,115,26,241,255,0,46,243,243,207,252,252,142,219,186,129,94,233,225,192,127,209,248,235,246,19,244,245,63,76,255,0,58,252,195,240,239,252,19,207,254,9,241,48,65,63,236,45,251,26,207,135,80,254,127,236,191,240,70,77,163,113,71,220,205,224,115,149,8,241,30,227,35,235,94,213,161,255,0,193,54,127,224,156,115,136,196,223,176,7,236,73,59,55,151,147,47,236,169,240,37,203,51,126,238,76,171,120,15,156,16,8,199,0,182,125,107,209,165,157,101,85,125,229,237,146,125,227,7,219,251,231,229,217,167,132,153,174,85,120,212,199,82,159,38,246,82,255,0,35,245,51,195,159,113,58,31,220,205,223,176,184,198,127,63,229,94,137,168,96,194,157,63,214,169,63,93,178,12,183,167,113,95,153,218,39,252,19,11,254,9,167,57,253,231,252,19,183,246,22,125,200,172,124,207,217,35,224,20,135,50,70,92,129,159,0,30,3,32,199,251,216,231,173,117,250,135,252,18,215,254,9,146,45,99,154,63,248,39,47,236,35,22,232,92,157,159,178,47,236,252,187,93,208,58,174,71,195,224,78,0,108,31,110,59,215,82,196,101,147,143,63,180,171,111,240,71,203,251,231,193,98,50,12,86,26,188,104,58,145,110,77,36,245,255,0,35,232,239,18,125,199,198,127,212,195,255,0,165,7,215,165,120,118,187,254,189,121,206,101,184,56,207,163,166,7,233,254,123,120,110,183,255,0,4,198,255,0,130,108,196,175,229,127,193,61,191,97,232,177,36,128,121,127,178,127,192,84,32,9,80,0,10,120,4,113,130,107,200,60,65,255,0,4,220,255,0,130,119,66,174,96,253,130,63,98,216,190,119,57,135,246,91,248,25,30,51,60,125,12,126,5,24,27,115,199,166,71,115,88,207,51,202,233,164,156,171,59,127,114,62,95,223,103,211,101,126,29,99,243,53,7,79,27,78,28,237,116,151,249,30,151,226,158,34,115,193,34,8,15,39,150,63,105,60,147,140,1,143,169,252,43,230,79,24,14,46,185,199,252,132,186,224,126,24,61,115,142,213,207,120,143,254,9,239,251,1,219,73,34,193,251,15,126,200,16,6,109,170,33,253,154,62,12,71,243,9,156,229,124,191,5,12,124,168,223,64,49,158,213,224,158,42,253,134,127,97,203,68,158,75,95,216,211,246,82,143,24,43,228,254,207,31,8,224,217,130,243,128,4,126,13,24,30,90,160,56,254,246,59,87,5,78,44,202,48,247,126,206,179,113,254,236,127,249,51,244,252,163,232,217,159,231,110,154,165,157,97,224,234,91,117,61,47,101,216,165,227,46,39,151,168,255,0,72,131,175,7,38,208,245,227,185,53,241,199,141,73,251,60,195,156,253,158,223,1,71,127,181,158,221,142,51,250,126,62,135,226,223,216,247,246,63,177,130,119,180,253,147,127,102,139,118,69,10,173,31,192,175,133,241,228,164,111,57,111,147,194,170,1,206,1,199,108,143,64,62,75,241,215,236,213,251,50,105,251,190,197,251,57,252,2,180,88,146,86,86,131,225,15,195,248,3,201,10,40,68,118,143,65,27,179,36,204,164,117,37,20,100,21,57,243,107,248,153,146,224,215,52,176,117,230,158,155,65,91,111,239,31,179,240,223,208,15,140,248,142,84,227,67,138,176,84,121,244,188,163,83,77,183,208,242,175,28,1,182,97,193,198,175,41,199,92,113,119,156,0,62,157,191,78,159,147,190,28,231,224,39,194,12,255,0,209,26,248,90,61,127,230,7,164,142,73,237,208,126,53,251,29,251,6,254,201,63,4,254,42,255,0,193,85,63,98,127,14,107,159,179,63,192,63,28,254,207,122,127,143,254,41,248,75,227,135,133,124,81,240,139,225,54,179,225,43,239,21,248,247,246,55,253,175,124,107,240,19,71,241,31,133,60,69,160,60,158,43,180,184,190,253,157,190,51,234,150,254,85,181,229,133,133,255,0,195,171,59,189,73,173,47,95,195,143,63,246,209,255,0,14,159,255,0,130,89,231,63,240,237,79,216,7,62,191,240,198,255,0,179,182,121,235,207,252,43,154,253,99,134,177,84,243,204,159,9,154,83,167,44,61,44,87,189,24,201,43,217,104,158,142,218,219,240,63,144,252,94,240,246,183,133,60,125,157,112,38,39,52,163,155,227,50,39,10,117,170,209,191,34,169,40,169,56,89,234,165,20,226,221,250,52,246,103,248,220,126,213,127,242,24,208,242,63,230,39,227,30,135,214,235,71,244,200,245,252,115,69,127,181,55,193,79,216,139,246,47,253,154,252,85,168,120,235,246,116,253,145,63,102,15,128,94,55,213,188,63,119,225,61,87,198,63,5,62,1,124,41,248,87,226,173,75,194,183,250,142,149,172,95,120,107,80,241,15,129,124,39,97,119,121,225,249,181,125,11,67,186,150,202,73,154,218,75,157,26,210,119,140,203,111,11,33,95,83,5,201,21,27,41,91,186,63,49,63,22,190,32,126,192,255,0,240,87,77,107,246,151,253,162,62,62,104,191,30,124,49,46,171,241,98,227,197,159,10,60,57,172,248,115,246,184,248,31,240,178,194,15,217,39,195,31,27,190,58,124,64,253,154,190,21,221,124,47,241,135,252,16,79,226,124,158,24,241,7,134,252,21,241,171,84,178,213,181,38,241,175,136,245,77,91,84,184,188,187,159,94,184,180,254,206,181,176,233,252,59,251,51,124,117,253,143,255,0,224,154,222,5,248,5,241,189,62,30,170,248,103,254,10,191,255,0,4,244,241,87,195,23,240,87,197,61,95,227,87,136,46,188,23,241,115,254,10,219,251,27,252,98,241,125,255,0,197,127,137,55,127,179,191,194,125,59,94,241,253,207,199,207,31,124,106,184,182,139,64,248,125,225,221,35,77,240,181,207,134,180,175,47,81,212,236,117,45,95,80,254,135,107,224,15,248,41,103,252,155,175,195,159,251,63,255,0,248,36,239,254,189,55,246,55,164,161,5,41,77,69,41,203,119,213,252,194,239,190,199,224,87,252,23,191,246,105,111,130,191,181,63,193,47,219,139,195,16,11,95,4,126,209,123,127,103,111,142,114,173,210,89,195,167,124,91,209,60,49,253,179,240,115,198,90,157,246,161,226,41,174,53,70,212,252,23,224,27,223,14,105,186,101,142,151,105,167,233,215,154,20,147,253,176,234,222,42,146,223,81,248,167,225,222,186,36,137,78,226,50,203,188,9,2,136,203,29,172,99,95,48,249,106,178,133,110,73,33,46,50,64,45,207,246,91,251,103,254,204,94,28,253,178,255,0,101,159,141,223,179,47,137,181,86,240,221,191,197,143,4,222,105,30,31,241,140,118,154,150,163,113,240,247,226,14,147,117,105,226,111,133,223,19,172,244,189,35,196,90,76,250,197,255,0,134,62,37,104,126,19,241,5,189,136,212,172,163,189,159,195,113,218,207,58,91,205,45,127,1,254,9,253,160,126,31,252,58,215,124,67,240,239,227,87,143,62,27,124,40,248,171,240,223,197,62,36,248,113,241,83,225,215,136,254,40,120,33,181,47,1,124,70,240,22,181,168,120,63,199,126,12,189,212,173,245,36,182,214,142,149,226,157,23,88,178,91,251,54,154,195,81,93,38,59,235,11,137,237,47,45,167,127,231,63,23,56,75,20,179,76,46,121,150,97,167,94,56,219,198,172,105,197,201,170,138,207,154,203,101,37,173,222,242,185,254,190,125,4,188,115,201,113,190,31,231,190,24,113,166,119,67,45,196,240,197,171,96,42,98,171,66,148,106,97,42,59,123,40,202,110,60,210,163,36,224,162,190,26,106,151,86,126,207,248,35,86,105,97,129,129,70,99,180,224,184,151,27,55,72,129,192,35,229,86,18,199,238,99,227,29,190,174,240,126,164,29,32,59,138,166,21,74,179,5,224,34,129,198,112,196,68,192,146,121,204,71,166,56,252,125,240,47,237,161,251,42,88,236,91,255,0,218,123,246,124,181,117,97,205,239,198,159,135,112,198,25,74,109,98,36,241,8,5,119,42,158,8,199,156,216,192,82,79,213,30,18,253,188,63,98,187,55,85,186,253,175,255,0,101,203,116,39,36,207,241,243,225,92,64,41,59,242,76,158,44,27,91,15,47,28,17,133,24,7,131,241,56,12,171,56,229,131,121,117,117,162,255,0,151,115,242,242,63,64,241,23,136,120,46,165,74,255,0,87,226,108,13,100,219,105,199,19,73,222,253,52,147,255,0,131,99,245,183,194,183,129,246,41,109,165,134,198,83,134,37,179,176,238,200,36,51,48,132,224,243,248,110,39,232,111,14,92,100,161,4,130,84,21,36,54,99,222,155,248,56,231,50,43,142,220,254,25,252,153,240,223,252,20,63,246,12,183,116,55,31,182,239,236,137,111,194,185,51,126,210,31,6,227,253,230,2,176,43,47,140,240,0,148,33,25,247,57,32,19,94,249,160,255,0,193,74,127,224,158,48,121,94,111,237,235,251,25,196,23,143,222,254,212,127,4,34,36,43,7,4,110,241,192,35,247,110,87,7,3,177,237,95,103,130,203,179,56,242,223,3,86,47,77,225,47,47,37,248,159,198,92,103,153,228,117,37,89,80,205,40,79,127,134,172,31,228,222,231,234,254,133,40,13,17,15,133,0,242,31,131,181,129,92,228,242,60,162,223,69,39,24,6,189,38,232,137,116,232,128,27,67,31,44,119,63,42,203,22,79,229,159,198,191,48,244,111,248,41,239,252,19,98,53,133,101,255,0,130,132,126,196,17,21,192,62,111,237,99,240,17,0,202,52,121,249,188,127,192,12,51,159,70,207,173,122,106,255,0,193,83,63,224,152,242,88,0,223,240,81,111,216,69,100,70,5,16,254,215,127,179,246,243,188,135,201,65,241,7,142,36,35,208,109,39,57,233,244,212,50,252,119,34,79,7,82,239,95,129,249,31,205,89,222,39,12,241,148,234,66,180,37,24,205,106,154,242,236,125,37,175,67,242,203,243,240,9,111,187,129,150,2,98,50,79,251,24,252,125,176,124,31,196,109,143,49,49,200,57,39,177,45,56,3,28,117,253,223,254,61,94,51,172,127,193,78,255,0,224,154,242,164,193,63,224,161,127,176,220,164,147,129,31,237,105,240,17,243,251,150,0,2,190,62,57,228,129,211,190,43,196,117,223,248,41,15,252,19,186,119,152,193,251,122,254,197,211,110,35,6,47,218,151,224,107,131,251,230,36,2,158,58,228,128,114,113,216,230,188,204,94,87,152,40,171,96,170,183,254,9,117,183,145,250,87,9,102,153,117,55,78,53,113,180,160,163,107,222,113,93,188,255,0,51,211,60,82,226,71,118,3,104,222,49,130,91,239,70,242,19,200,224,141,228,122,124,188,26,249,143,198,51,121,113,202,161,65,243,0,231,32,99,49,71,23,247,78,64,243,115,215,248,5,114,94,34,255,0,130,133,126,193,146,150,251,47,237,189,251,33,76,25,29,128,135,246,147,248,51,49,105,4,72,168,51,31,141,58,238,44,0,239,235,193,53,243,191,138,255,0,110,175,216,174,237,138,219,126,216,31,178,221,208,222,126,120,127,104,63,132,178,168,5,248,207,149,226,211,184,109,133,1,231,63,54,115,147,207,200,99,242,156,214,242,113,203,235,74,253,169,203,203,200,254,181,224,62,38,225,186,85,40,42,217,230,18,156,99,107,243,87,166,154,181,187,200,151,199,151,4,163,141,225,55,59,33,140,63,44,55,162,112,160,242,74,36,164,15,238,238,4,99,38,190,26,248,147,168,36,105,52,206,248,136,162,188,138,24,159,33,247,203,118,196,170,174,75,121,113,166,64,25,193,4,240,49,93,199,143,63,108,159,217,50,248,72,44,127,105,255,0,217,242,233,152,113,246,47,141,159,13,110,10,51,9,8,10,97,241,62,73,13,112,188,250,198,199,28,124,223,25,252,72,253,163,62,10,106,250,103,138,111,252,53,241,111,225,255,0,137,173,188,57,225,253,83,196,190,32,95,9,120,183,66,241,124,246,30,26,209,224,140,235,26,188,218,127,134,111,111,39,107,56,108,162,151,50,44,12,119,93,69,28,121,150,88,213,190,43,29,144,231,85,235,83,164,242,218,202,51,148,85,221,57,164,174,210,213,219,254,24,254,231,240,243,196,46,2,203,114,252,70,50,124,87,128,115,195,81,169,87,145,98,168,243,63,103,7,43,37,207,119,39,109,15,215,127,248,37,63,194,54,210,239,191,98,95,218,75,88,240,239,217,181,175,143,159,240,87,95,137,254,11,240,119,141,6,176,179,175,139,254,10,254,206,223,240,73,159,248,41,55,130,236,52,241,225,203,77,85,225,208,63,177,191,105,157,127,246,179,180,243,174,108,44,181,109,68,207,231,201,37,238,128,158,26,153,63,177,74,252,42,248,85,240,19,83,253,153,62,15,127,193,191,31,6,124,75,225,31,248,65,126,33,248,123,246,160,241,6,189,241,155,194,231,95,131,196,231,79,253,160,126,35,127,193,43,63,224,166,31,18,255,0,104,121,255,0,182,172,53,189,70,202,235,237,127,28,60,95,241,2,243,102,149,121,46,137,111,253,161,246,109,13,97,210,33,178,130,47,221,90,254,207,201,112,16,203,50,156,187,1,8,242,44,37,26,112,105,119,81,92,221,23,218,190,234,253,245,63,231,211,196,126,40,173,198,188,123,198,28,87,94,179,175,44,247,49,197,98,35,41,59,183,78,117,101,236,83,124,210,218,146,130,210,77,105,238,233,96,162,138,43,211,62,40,43,224,15,248,41,103,252,155,175,195,159,251,63,255,0,248,36,247,235,255,0,5,77,253,141,235,239,250,248,3,254,10,89,255,0,38,235,240,231,254,207,255,0,254,9,59,255,0,175,77,253,141,232,3,239,250,255,0,32,207,219,19,198,247,218,39,252,20,175,254,10,49,100,254,32,240,111,128,236,98,253,191,191,109,67,31,139,188,119,101,226,11,189,22,230,79,248,105,143,140,172,154,68,98,105,116,61,55,237,210,125,166,118,143,203,215,231,156,38,153,38,44,164,255,0,72,107,47,245,243,175,230,227,226,135,252,27,7,251,21,252,81,248,227,241,171,227,237,207,237,41,251,111,248,75,197,159,29,126,48,124,80,248,215,226,189,27,194,126,46,253,153,79,133,180,175,21,124,90,241,199,137,124,127,226,93,63,195,86,190,44,253,149,53,91,235,95,15,197,173,248,175,86,75,40,175,53,11,219,136,173,140,113,205,119,113,34,25,91,58,180,227,82,60,178,138,151,175,252,51,52,165,90,165,25,115,82,169,42,109,232,220,91,78,223,38,143,228,23,246,123,189,241,87,197,95,182,88,120,123,246,163,240,5,205,150,137,246,105,245,27,159,217,255,0,194,30,13,111,22,105,247,119,163,90,93,50,219,93,212,124,127,226,223,30,105,182,250,21,204,54,250,171,121,35,68,181,190,154,107,56,158,223,82,138,27,123,187,123,207,211,63,1,124,16,248,155,61,230,34,253,177,63,104,235,34,38,198,235,95,12,126,200,205,192,150,204,3,139,223,217,98,81,252,64,242,58,168,207,124,254,213,107,159,240,107,55,236,81,226,164,211,99,241,127,237,47,251,98,248,210,61,27,237,159,217,17,248,219,69,253,129,124,100,154,95,246,129,136,223,141,61,60,79,251,7,221,139,49,57,183,182,50,136,246,249,134,214,34,249,49,71,182,189,175,252,26,167,251,0,90,29,209,124,84,248,224,231,57,255,0,73,248,25,255,0,4,187,188,92,229,79,221,188,255,0,130,115,56,199,200,56,198,49,145,208,156,241,75,2,223,195,53,15,46,72,190,222,75,177,179,198,98,26,73,214,156,173,253,249,127,153,242,31,195,239,217,223,226,244,233,105,228,254,222,63,181,101,137,204,12,62,205,224,255,0,216,122,77,191,241,226,62,95,182,126,198,146,243,130,156,228,143,144,103,57,57,245,95,23,248,159,198,191,178,147,120,109,252,95,255,0,5,80,248,35,224,1,227,216,47,191,179,143,252,20,79,225,231,192,11,200,245,35,225,120,44,133,216,248,66,62,7,248,235,246,125,49,136,15,136,173,87,95,254,213,111,23,110,251,110,134,108,142,133,139,207,237,191,162,45,127,224,215,191,216,74,211,30,79,196,207,138,13,140,127,199,207,236,193,255,0,4,145,188,233,183,175,219,63,224,153,178,100,124,163,57,235,147,158,167,63,65,252,26,255,0,130,21,124,40,253,157,91,95,127,128,31,181,255,0,237,59,240,73,252,84,52,133,241,67,252,39,248,25,255,0,4,167,248,124,222,37,30,31,93,77,52,47,248,72,27,194,191,240,77,75,83,173,27,53,214,245,159,178,253,167,205,48,127,107,92,152,182,153,229,221,135,246,109,91,221,214,140,227,219,146,42,251,117,179,181,189,58,16,235,205,173,101,38,252,229,47,243,62,65,248,99,255,0,5,11,248,163,97,226,143,14,104,255,0,14,62,41,126,200,127,240,83,200,60,71,174,233,90,111,136,180,255,0,216,75,193,95,29,252,55,226,159,131,208,220,106,63,101,178,183,241,46,167,240,247,94,248,255,0,224,75,93,123,197,9,123,168,175,134,231,248,175,227,223,128,94,17,183,184,240,46,161,37,247,140,103,209,71,136,53,207,5,122,102,177,251,85,252,110,215,255,0,99,63,143,63,240,81,38,255,0,130,128,124,10,248,19,255,0,10,92,124,80,54,31,179,127,142,254,25,248,67,82,253,156,190,9,253,155,83,146,251,225,151,236,243,255,0,5,19,208,244,101,215,62,52,104,255,0,183,133,159,135,60,113,224,109,35,198,218,15,128,188,113,225,159,236,31,30,106,58,54,131,161,124,53,241,159,246,117,205,175,196,127,208,43,63,248,39,111,197,77,60,131,105,255,0,5,70,253,191,161,32,146,15,252,33,95,240,76,153,15,59,193,230,95,248,39,27,113,243,191,29,62,106,252,158,241,215,236,93,251,95,234,26,71,193,159,218,218,231,227,127,237,73,226,191,219,15,67,255,0,130,133,234,159,178,30,159,227,253,99,246,38,255,0,130,110,107,191,24,62,26,126,194,179,127,193,67,190,43,254,206,82,124,88,240,175,139,101,255,0,130,111,55,138,188,25,160,79,251,34,107,23,190,53,147,93,143,83,79,7,74,158,63,212,188,71,253,153,255,0,8,190,170,246,13,159,246,100,211,77,40,65,233,119,189,213,214,150,229,81,141,246,186,87,210,250,130,172,214,242,110,219,121,61,60,255,0,59,157,111,199,15,248,40,23,197,251,95,28,107,158,16,241,213,183,193,95,248,38,119,134,116,145,166,92,233,191,22,63,111,255,0,14,252,80,241,158,159,226,29,86,231,72,210,110,83,225,87,246,167,129,245,95,3,252,10,186,241,198,161,5,254,163,173,233,207,224,159,218,119,226,54,181,14,137,224,235,200,53,63,5,89,106,171,226,136,60,9,227,30,16,241,63,141,63,106,214,241,28,158,15,255,0,130,168,124,18,248,128,190,3,130,192,234,13,255,0,4,236,248,121,240,10,198,61,56,120,158,222,244,217,143,139,255,0,240,188,252,117,251,64,239,243,255,0,225,28,187,26,7,246,83,120,72,175,216,245,207,183,127,110,230,207,251,23,246,110,247,254,9,219,241,83,81,255,0,143,207,248,42,55,237,253,54,113,215,193,95,240,76,148,228,109,231,247,127,240,78,49,131,242,142,107,230,255,0,140,191,240,66,175,133,63,180,91,104,13,241,255,0,246,191,253,167,126,55,55,133,6,172,60,44,223,22,62,6,127,193,41,254,32,159,13,13,121,116,180,215,63,225,31,62,43,255,0,130,106,93,127,98,155,197,209,52,111,181,27,111,40,220,127,100,91,121,187,252,136,182,218,203,36,146,81,113,165,110,170,211,237,252,209,191,254,77,232,17,175,40,181,105,61,60,218,237,230,126,78,252,65,253,157,126,47,64,151,126,111,237,225,251,87,94,129,246,141,223,105,240,135,236,56,129,240,47,186,253,147,246,51,135,131,180,240,49,247,142,49,198,62,27,241,247,193,15,137,176,94,17,39,237,135,251,70,222,177,151,239,220,248,103,246,71,70,255,0,93,122,1,31,99,253,150,97,31,121,91,183,252,180,61,177,183,247,14,231,254,13,122,253,132,174,195,9,126,38,124,80,64,196,146,45,127,102,15,248,36,141,151,93,217,193,178,255,0,130,102,71,183,239,28,99,24,192,199,65,142,122,231,254,13,83,253,128,46,142,101,248,171,241,193,78,115,254,141,240,55,254,9,121,102,51,146,126,237,159,252,19,154,49,143,152,241,140,112,7,64,49,113,203,170,43,95,16,154,95,244,238,35,88,154,209,119,85,36,159,248,153,252,141,254,208,30,33,214,62,23,106,139,225,141,111,246,180,248,101,21,221,221,141,182,182,182,159,31,60,57,224,67,227,233,32,186,158,27,56,231,211,95,225,239,140,124,7,99,255,0,8,169,109,50,69,183,18,104,87,55,63,106,181,212,55,234,147,199,228,219,88,254,50,254,210,126,58,213,117,191,14,107,54,233,119,161,120,170,197,45,237,12,30,59,240,85,182,189,101,225,205,76,54,165,225,191,55,236,177,234,16,221,233,254,76,14,205,101,50,233,254,34,214,46,62,213,27,25,109,237,23,237,17,89,127,164,182,141,255,0,6,190,126,199,126,26,210,167,208,188,43,251,83,126,218,94,16,209,46,111,165,212,238,116,127,6,216,126,193,190,16,210,231,212,167,134,198,218,125,66,109,63,195,95,176,165,172,82,223,61,190,153,167,198,242,178,25,25,44,227,70,98,170,5,121,111,141,63,224,209,159,216,11,226,29,164,214,30,48,253,168,255,0,111,253,94,210,227,203,243,97,30,57,253,149,52,253,222,84,150,146,161,15,165,254,199,240,50,16,246,54,167,33,134,68,88,57,12,193,186,105,225,35,30,94,120,198,110,61,121,99,174,221,44,105,28,195,27,79,248,88,170,148,244,182,147,146,209,244,209,159,180,255,0,182,71,252,156,87,252,18,123,254,207,255,0,226,55,254,186,203,254,10,89,95,127,215,192,31,182,71,252,156,87,252,18,119,254,207,255,0,226,63,254,186,203,254,10,89,95,127,215,105,198,20,81,69,0,21,240,7,252,20,179,254,77,215,225,207,253,159,255,0,252,18,119,255,0,94,155,251,27,215,223,245,252,141,248,159,254,29,83,255,0,15,145,255,0,130,151,143,248,40,87,252,59,231,251,124,252,91,248,17,255,0,8,249,253,179,63,225,156,134,177,255,0,8,192,253,128,63,99,179,167,255,0,99,31,141,234,102,254,193,254,218,254,221,242,124,143,244,127,181,125,175,103,239,124,234,195,17,91,234,244,165,87,217,202,175,47,217,138,187,99,138,230,105,94,215,63,174,74,43,248,201,248,33,226,207,216,15,195,127,240,92,255,0,216,87,71,255,0,130,116,120,151,246,62,208,116,79,31,120,159,246,145,240,167,196,237,47,246,41,214,126,11,233,122,95,141,126,27,217,254,200,95,30,252,115,103,225,159,30,89,124,11,185,72,124,71,224,104,126,35,120,47,192,90,196,118,90,130,205,96,154,223,132,180,125,65,98,23,246,22,82,195,253,155,83,161,91,219,210,141,94,73,83,230,251,50,86,107,213,4,151,43,106,247,176,81,69,21,176,130,138,40,160,2,138,40,160,2,138,40,160,2,138,40,160,2,138,43,224,15,248,43,15,252,162,203,254,10,89,255,0,102,1,251,100,127,235,58,252,70,160,3,246,200,255,0,147,138,255,0,130,79,127,217,255,0,252,70,199,254,42,203,254,10,89,95,127,215,241,109,143,248,55,227,254,24,159,7,254,28,226,126,46,249,35,24,31,177,73,248,144,88,219,240,79,252,196,195,111,0,113,206,238,122,156,87,237,183,252,27,255,0,226,203,159,26,127,193,38,255,0,102,157,98,79,19,79,226,205,46,207,197,127,181,71,132,252,27,170,190,179,38,187,167,218,252,53,240,23,237,133,241,247,192,191,11,60,51,225,155,214,185,150,56,60,13,162,252,55,240,231,133,180,111,15,217,90,184,211,244,237,15,65,211,244,253,58,56,108,45,109,161,143,147,13,138,250,196,170,71,216,206,151,179,118,247,149,147,243,93,209,114,135,42,139,186,119,63,100,232,162,138,235,32,255,217
        };
    std::string strOsgData;
    for( size_t i = 0; i < 8230; ++i )
    {
        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );
    }
    return strOsgData;
}
#endif

