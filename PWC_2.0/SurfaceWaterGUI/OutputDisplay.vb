Public Class OutputDisplay

    Private Sub New()
        'Empty constructor to prevent instantiation of this purely static class
    End Sub

    Public Shared ReadOnly Display1 As Object() = {Form1.Chart1, Form1.ParentOutput, Form1.Deg1Output, Form1.Deg2Output,
                                                   Form1.PeakOutputLab1, Form1.Peak90thOutput1, Form1.FourDay90th, Form1.Year90th,
                                                   Form1.BenthicPeakTB, Form1.Benthic21DayTB, Form1.RunoffText1, Form1.ErosionText1,
                                                   Form1.DriftText1, Form1.Field2WaterText, Form1.BurialText, Form1.Metabolism2Text,
                                                   Form1.Hydrolysis2Text, Form1.TotalDeg2Text, Form1.WashoutText, Form1.Metabolism1Text,
                                                   Form1.Hydrolysis1Text, Form1.Photolysis1Text, Form1.Volatiliz1Text, Form1.TotalDeg1Text,
                                                   Form1.SimMean, Form1.SedimentPeak1, Form1.Sediment21Day1, Form1.BioAvailFraction1,
                                                   Form1.TwentyOneDay90th, Form1.SixtyDay90th, "USEPA standard pond", Form1.PondCropAreaLabel,
                                                   Form1.OneDayReturn1}

    Public Shared ReadOnly Display2 As Object() = {Form1.Chart2, Form1.ParentGraph2, Form1.Degradate1Graph2, Form1.Degradate2Graph2,
                                                   Form1.PeakOutputLab2, Form1.Peak90thOutput2, Form1.FourDay90th2, Form1.Year90th2,
                                                   Form1.BenthicPeakTB2, Form1.Benthic21DayTB2, Form1.RunoffText2, Form1.ErosionText2,
                                                   Form1.DriftText2, Form1.Field2WaterText2, Form1.BurialText2, Form1.Metabolism2Text2,
                                                   Form1.Hydrolysis2Text2, Form1.TotalDeg2Text2, Form1.WashoutText2, Form1.Metabolism1Text2,
                                                   Form1.Hydrolysis1Text2, Form1.Photolysis1Text2, Form1.VolatileText2, Form1.TotalDeg1Text2,
                                                   Form1.SimMean2, Form1.SedimentPeak2, Form1.Sediment21Day2, Form1.BioAvailFraction2,
                                                   Form1.TwentyOneDay90th2, Form1.SixtyDay90th2, "USEPA standard reservoir", Form1.ReservoirCroppedAreaBox,
                                                   Form1.OneDayReturn2}

    Public Shared ReadOnly Display3 As Object() = {Form1.Chart3, Form1.ParentGraph3, Form1.Degradate1Graph3, Form1.Degradate2Graph3,
                                                   Form1.PeakOutputLab3, Form1.Peak90thOutput3, Form1.FourDay90th3, Form1.Year90th3,
                                                   Form1.BenthicPeakTB3, Form1.Benthic21DayTB3, Form1.RunoffText3, Form1.ErosionText3,
                                                   Form1.DriftText3, Form1.Field2WaterText3, Form1.BurialText3, Form1.Metabolism2Text3,
                                                   Form1.Hydrolysis2Text3, Form1.TotalDeg2Text3, Form1.WashoutText3, Form1.Metabolism1Text3,
                                                   Form1.Hydrolysis1Text3, Form1.Photolysis1Text3, Form1.VolatileText3, Form1.TotalDeg1Text3,
                                                   Form1.SimMean3, Form1.SedimentPeak3, Form1.Sediment21Day3, Form1.BioAvailFraction3,
                                                   Form1.TwentyOneDay90th3, Form1.SixtyDay90th3, "User-defined water body", Form1.CustomCroppedAreaBox,
                                                   Form1.OneDayReturn3}




End Class
