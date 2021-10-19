{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTVM.SCP.Tmp where

import           Data.String    (IsString(..))

import           GTVM.SCP
import           GTVM.SCP.SCPX

import           Data.WorldPeace

ex05 :: IsString bs => SCPSeg05Textbox bs
ex05 = SCPSeg05Textbox' 5 5 "hi from banri" "" 0
ex05' :: IsString bs => SCPSegment bs
ex05' = SCPSeg05Textbox ex05
exXT :: SCPXText
exXT = SCPXText' "hi from banri" SpeakerBanri Nothing

exT :: IsString bs => [SCPX bs]
exT = [SCPXPrimitive ex05', SCPXText exXT, SCPXPrimitive ex05']

testWPText :: OpenUnion '[SCPSegment bs, SCPXText]
testWPText = openUnionLift exXT

testWPText' :: OpenUnion '[SCPSegment bs, [SCPSegment bs], SCPXText]
testWPText' = openUnionLift exXT

testWPPrim :: forall bs. IsString bs => OpenUnion '[SCPSegment bs, SCPXText]
testWPPrim = openUnionLift (ex05' @bs)

testWPPrim' :: forall bs. IsString bs => OpenUnion '[SCPSegment bs, [SCPSegment bs], SCPXText]
testWPPrim' = openUnionLift (ex05' @bs)

testWPPrimList :: forall bs. IsString bs => OpenUnion '[SCPSegment bs, [SCPSegment bs], SCPXText]
testWPPrimList = openUnionLift [ex05' @bs, ex05' @bs]

testWPList :: forall bs. IsString bs => [OpenUnion '[SCPSegment bs, SCPXText]]
testWPList = [testWPPrim, testWPText, testWPPrim]

testWPList' :: forall bs. IsString bs => [OpenUnion '[SCPSegment bs, [SCPSegment bs], SCPXText]]
testWPList' = [testWPPrimList, testWPText', testWPPrim']

testWPRemove
    :: forall bs. IsString bs
    => [OpenUnion '[SCPSegment bs, SCPXText]]
    -> [OpenUnion '[SCPSegment bs]]
testWPRemove = map testWPRemove'

testWPRemove'
    :: forall bs. IsString bs
    => OpenUnion '[SCPSegment bs, SCPXText]
    -> OpenUnion '[SCPSegment bs]
testWPRemove' = openUnionStripTransform f
  where
    f :: SCPXText -> OpenUnion '[SCPSegment bs]
    f x = openUnionLift @(SCPSegment bs) (SCPSeg05Textbox (scpXTextTransform x))

-- | Strip a type from an 'OpenUnion'. If the value is of the type to strip,
--   convert it using the provided function.
openUnionStripTransform
    :: forall a as. ElemRemove a as
    => (a -> OpenUnion (Remove a as)) -> OpenUnion as -> OpenUnion (Remove a as)
openUnionStripTransform = openUnionHandle id
