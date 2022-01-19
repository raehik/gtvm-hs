{-# LANGUAGE OverloadedStrings #-}

module GTVM.SCP.Tmp where

import           Data.String    (IsString(..))

import           GTVM.SCP
import           GTVM.SCP.SCPX

import           Data.WorldPeace

ex05 :: IsString bs => SCPSeg05Textbox bs
ex05 = SCPSeg05Textbox' 5 5 "hi from banri" "" 0
ex05' :: IsString bs => SCPSeg bs
ex05' = SCPSeg05Textbox ex05
exXT :: SCPXSegText
exXT = SCPXSegText' "hi from banri" SpeakerBanri Nothing

exT :: IsString bs => SCPX bs
exT = [SCPXSegPrimitive ex05', SCPXSegText exXT, SCPXSegPrimitive ex05']

testWPText :: OpenUnion '[SCPSeg bs, SCPXSegText]
testWPText = openUnionLift exXT

testWPText' :: OpenUnion '[SCPSeg bs, [SCPSeg bs], SCPXSegText]
testWPText' = openUnionLift exXT

testWPPrim :: forall bs. IsString bs => OpenUnion '[SCPSeg bs, SCPXSegText]
testWPPrim = openUnionLift (ex05' @bs)

testWPPrim' :: forall bs. IsString bs => OpenUnion '[SCPSeg bs, [SCPSeg bs], SCPXSegText]
testWPPrim' = openUnionLift (ex05' @bs)

testWPPrimList :: forall bs. IsString bs => OpenUnion '[SCPSeg bs, [SCPSeg bs], SCPXSegText]
testWPPrimList = openUnionLift [ex05' @bs, ex05' @bs]

testWPList :: forall bs. IsString bs => [OpenUnion '[SCPSeg bs, SCPXSegText]]
testWPList = [testWPPrim, testWPText, testWPPrim]

testWPList' :: forall bs. IsString bs => [OpenUnion '[SCPSeg bs, [SCPSeg bs], SCPXSegText]]
testWPList' = [testWPPrimList, testWPText', testWPPrim']

testWPRemove
    :: forall bs. IsString bs
    => [OpenUnion '[SCPSeg bs, SCPXSegText]]
    -> [OpenUnion '[SCPSeg bs]]
testWPRemove = map testWPRemove'

testWPRemove'
    :: forall bs. IsString bs
    => OpenUnion '[SCPSeg bs, SCPXSegText]
    -> OpenUnion '[SCPSeg bs]
testWPRemove' = openUnionStripTransform f
  where
    f :: SCPXSegText -> OpenUnion '[SCPSeg bs]
    f x = openUnionLift @(SCPSeg bs) (SCPSeg05Textbox (scpXSegTextTransform x))

-- | Strip a type from an 'OpenUnion'. If the value is of the type to strip,
--   convert it using the provided function.
openUnionStripTransform
    :: forall a as. ElemRemove a as
    => (a -> OpenUnion (Remove a as)) -> OpenUnion as -> OpenUnion (Remove a as)
openUnionStripTransform = openUnionHandle id
