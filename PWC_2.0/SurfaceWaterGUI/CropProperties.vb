Public Class CropProperties
    Private Sub New()
        'Empty constructor to prevent instantiation of this purely static class
    End Sub

    Public Const maxCropCycles As Integer = 7


    Public Shared ReadOnly emergenceDay As TextBox() = {Form1.dayEmerge1, Form1.dayEmerge2, Form1.dayEmerge3, Form1.dayEmerge4, Form1.dayEmerge5, Form1.dayEmerge6, Form1.dayEmerge7}
    Public Shared ReadOnly emergenceMonth As TextBox() = {Form1.monthEmerge1, Form1.monthEmerge2, Form1.monthEmerge3, Form1.monthEmerge4, Form1.monthEmerge5, Form1.monthEmerge6, Form1.monthEmerge7}

    Public Shared ReadOnly maturityDay As TextBox() = {Form1.dayMature1, Form1.dayMature2, Form1.dayMature3, Form1.dayMature4, Form1.dayMature5, Form1.dayMature6, Form1.dayMature7}
    Public Shared ReadOnly maturityMonth As TextBox() = {Form1.monthMature1, Form1.monthMature2, Form1.monthMature3, Form1.monthMature4, Form1.monthMature5, Form1.monthMature6, Form1.monthMature7}

    Public Shared ReadOnly harvestDay As TextBox() = {Form1.dayHarvest1, Form1.dayHarvest2, Form1.dayHarvest3, Form1.dayHarvest4, Form1.dayHarvest5, Form1.dayHarvest6, Form1.dayHarvest7}
    Public Shared ReadOnly harvestMonth As TextBox() = {Form1.monthHarvest1, Form1.monthHarvest2, Form1.monthHarvest3, Form1.monthHarvest4, Form1.monthHarvest5, Form1.monthHarvest6, Form1.monthHarvest7}

    Public Shared ReadOnly rootDepth As TextBox() = {Form1.rootDepth1, Form1.rootDepth2, Form1.rootDepth3, Form1.rootDepth4, Form1.rootDepth5, Form1.rootDepth6, Form1.rootDepth7}
    Public Shared ReadOnly canopyCover As TextBox() = {Form1.canopyCover1, Form1.canopyCover2, Form1.canopyCover3, Form1.canopyCover4, Form1.canopyCover5, Form1.canopyCover6, Form1.canopyCover7}

    Public Shared ReadOnly canopyHeight As TextBox() = {Form1.canopyHeight1, Form1.canopyHeight2, Form1.canopyHeight3, Form1.canopyHeight4, Form1.canopyHeight5, Form1.canopyHeight6, Form1.canopyHeight7}
    Public Shared ReadOnly canopyHoldup As TextBox() = {Form1.canopyHoldup1, Form1.canopyHoldup2, Form1.canopyHoldup3, Form1.canopyHoldup4, Form1.canopyHoldup5, Form1.canopyHoldup6, Form1.canopyHoldup7}

    Public Shared ReadOnly foliarDisposition As RadioButton() = {Form1.mb11, Form1.mb12, Form1.mb13,
                                                                 Form1.mb21, Form1.mb22, Form1.mb23,
                                                                 Form1.mb31, Form1.mb32, Form1.mb33,
                                                                 Form1.mb41, Form1.mb42, Form1.mb43,
                                                                 Form1.mb51, Form1.mb52, Form1.mb53,
                                                                 Form1.mb61, Form1.mb62, Form1.mb63,
                                                                 Form1.mb71, Form1.mb72, Form1.mb73}

    Public Shared ReadOnly foliarPanel As Panel() = {Form1.foliarpanel1, Form1.foliarpanel2, Form1.foliarpanel3, Form1.foliarpanel4, Form1.foliarpanel5, Form1.foliarpanel6, Form1.foliarpanel7}



    Public Shared ReadOnly plantingFrequency As TextBox() = {Form1.PlantFreq1, Form1.PlantFreq2, Form1.PlantFreq3, Form1.PlantFreq4, Form1.PlantFreq5, Form1.PlantFreq6, Form1.PlantFreq7}
    Public Shared ReadOnly plantingLag As TextBox() = {Form1.Lag1, Form1.Lag2, Form1.Lag3, Form1.Lag4, Form1.Lag5, Form1.Lag6, Form1.Lag7}

   


End Class
