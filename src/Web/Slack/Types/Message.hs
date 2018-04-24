{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Web.Slack.Types.Message where

import Control.Lens (makeLenses,(^.))
import Data.Aeson.TH (deriveToJSON, deriveFromJSON, defaultOptions,fieldLabelModifier)
import Data.Aeson.Types (FromJSON(..), Object, Parser, ToJSON(..), Value (..), KeyValue,
     (.:?), (.:), (.=), object, withObject)
import Control.Applicative (optional, (<|>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text, take, pack, toLower)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)
import Web.Slack.Types.Base (URL)
import Web.Slack.Types.Id (BotId, UserId, CommentId, ChannelId,TeamId)
import Web.Slack.Types.Time (SlackTimeStamp)
import Web.Slack.Utils (toSnake)

data MessagePayload = MessagePayload
    { messageId      :: Int
    , messageType    :: T.Text
    , messageChannel :: ChannelId
    , messageText    :: T.Text
    } deriving Show

data ChatMessage = ChatMessage
    { _msgType   :: T.Text
    , _msgUserId :: Maybe UserId
    , _msgText   :: T.Text
    , _msgTs     :: SlackTimeStamp
    } deriving (Show)

defaultToEmpty :: Object -> T.Text -> Parser T.Text
defaultToEmpty o tag = fmap (fromMaybe $ T.pack "") $ o .:? tag

data PingPayload = PingPayload
    { pingId        :: Int
    , pingType      :: T.Text
    , pingTimestamp :: Int
    } deriving (Show)

-- | Attachment to message. 

data Attachment = Attachment
    { _attachmentFallback    :: T.Text
        -- ^ A plain-text summary of the attachment.
    , _attachmentColor       :: Maybe AttachmentColor
        -- ^ Used to color the border along the left side of the message
        -- attachment.
    , _attachmentCallback_id :: T.Text 
        -- ^ The provided string will act as a unique identifier 
        -- for the collection of buttons within the attachment. 
        -- It will be sent back to your message button action URL with each invoked action. 
        -- This field is required when the attachment contains message buttons. 
        -- It is key to identifying the interaction you're working with.
    , _attachmentPretext     :: Maybe T.Text
        -- ^ Optional text that appears above the message attachment block.
    , _attachmentAuthorName  :: Maybe T.Text
        -- ^ Small text used to display the author's name.
    , _attachmentAuthorLink  :: Maybe URL
        -- ^ A valid URL that will hyperlink the author_name text mentioned
        -- above.
    , _attachmentAuthorIcon  :: Maybe URL
        -- ^ A valid URL that displays a small 16x16px image to the left of
        -- the author_name text.
    , _attachmentTitle       :: Maybe T.Text
        -- ^ The title is displayed as larger, bold text near the top of
        -- a message attachment.
    , _attachmentTitleLink   :: Maybe URL
        -- ^ By passing a valid URL, the title text will be hyperlinked.
    , _attachmentText        :: Maybe T.Text
        -- ^ This is the main text in a message attachment, and can contain
        -- standard message markup.
    , _attachmentFields      :: [Field]
    , _attachmentImageUrl    :: Maybe URL
        -- ^ An image file that will be displayed inside a message
        -- attachment. GIF, JPEG, PNG, or BMP; scaled down to 400x500px.
    , _attachmentThumbUrl     :: Maybe URL
        -- ^ Displayed as a thumbnail on the right side of a message
        -- attachment. GIF, JPEG, PNG, or BMP; scaled down to 75x75px.
    , _attachmentFooter      :: Maybe T.Text
        -- ^ Add some brief text to help contextualize and identify an
        -- attachment.
    , _attachmentFooterIcon  :: Maybe URL
        -- ^ Render a small icon beside your footer text. Scaled to 16x16px.
    , _attachmentTs          :: Maybe POSIXTime
        -- ^ Display an additional timestamp value as part of the
        -- attachment's footer.
    , _attachmentActions     :: [Action]
        -- ^ A collection of actions (buttons or menus) to include in the attachment. 
    }

data Field = Field
    { _fieldTitle :: Maybe T.Text
        -- ^ Shown as a bold heading above the value text. It cannot
        -- contain markup and will be escaped for you.
    , _fieldValue :: T.Text
        -- ^ The text value of the field. It may contain standard message
        -- markup and must be escaped as normal. May be multi-line.
    , _fieldShort :: Bool
        -- ^ Whether the value is short enough to be displayed side-by-side
        -- with other values.
    }

data AttachmentColor
    = DefaultColor       -- grey
    | GoodColor          -- green
    | WarningColor       -- yellow
    | DangerColor        -- red
    | CustomColor T.Text -- hexadecimal RGB colour, eg. CustomColor "#439FE0"
    deriving (Generic, Show)

-- | Data Source for action. You can use Users or Channels to use usernames or channel names in buttons  
--  or menu list. Static is used when all fields provided directly in posted message. 
-- External means that all options will be asked from your Option Load URL. 
data DataSource = Static | Users | Channels | Conversations | External
    deriving Show

-- | Main type for the Action. It can have Button or Select type. 
data Action = ActionButton Button | ActionSelect Select 
    deriving Show 

-- | Data type for short messages (in buttons or in menu). 
-- newtype ShortText = ShortText { getText :: T.Text } 
--    deriving (Eq, Show,FromJSON)
 
-- | Fuction to create short text. It takes only first 30 symbols.  
--makeShort :: T.Text -> ShortText
--makeShort = ShortText . T.take 30

-- | Select is an action with menu with list of options, which can be choosed by user. 
-- complete description of fields you can read in slack api docs : 
-- https://api.slack.com/docs/interactive-message-field-guide
data Select = Select  
    { _selectName             :: T.Text 
    , _selectText             :: T.Text
    , _selectValue            :: Maybe T.Text 
    , _selectConfirm          :: Maybe Confirm
    , _selectData_Source      :: Maybe DataSource
    , _selectSelected_options :: Maybe [OptField]
    , _selectMin_query_length :: Maybe Int
    , _selectOptions          :: Maybe [OptField]
    , _selectOption_groups    :: Maybe [OptGroup]
    } 
    deriving Show
-- | Buttin is an action with buttotns, which can be clicked by user. 
-- complete description of fields you can read in slack api docs : 
-- https://api.slack.com/docs/interactive-message-field-guide
data Button = Button 
    { _buttonName             :: T.Text
    , _buttonText             :: T.Text
    , _buttonValue            :: Maybe T.Text 
    , _buttonConfirm          :: Maybe Confirm
    , _buttonData_Source      :: Maybe DataSource
    , _buttonMin_query_length :: Maybe Int
    , _buttonStyle            :: Maybe ButtonStyle
    } 
    deriving Show 

-- | Stythis decorates buttons with extra visual importance, 
-- which is especially useful when providing logical default action or highlighting a destructive activity.
-- Default — Yes, it's the default. Buttons will look simple.
-- Primary — Use this sparingly, when the button represents a key action to accomplish. 
--   You should probably only ever have one primary button within a set.
-- Danger — Use this when the consequence of the button click will result in the destruction of something, 
--   like a piece of data stored on your servers. Use even more sparingly than primary.
data ButtonStyle = Default | Primary | Danger
    deriving Show

-- | Type of confirmation message. 
-- Protect users from destructive actions or particularly distinguished decisions 
-- by asking them to confirm their button click one more time. 
-- Use confirmation dialogs with care.
data Confirm = Confirm
    { _confirmTitle        :: Maybe T.Text 
    , _confirmText         :: T.Text
    , _confirmOk_text      :: Maybe T.Text
    , _confirmDismiss_text :: Maybe T.Text 
    }
    deriving Show

-- | Type of option field. Used in static and external message menu data types.
-- The value is especially important when used in selected options.
data OptField = OptField 
    { _optFieldText        :: T.Text
    , _optFieldValue       :: T.Text
    , _optFieldDescription :: Maybe T.Text
    }
    deriving Show

-- | Options group to place within message menu actions
-- Options group are a set of 100 options divided into groups. 
-- They can be used with static or external  data source types.
data OptGroup = OptGroup 
    { _groupText    :: T.Text
    , _groupOptions :: [OptField]
    }
    deriving Show

-- | data type for request for options from slack. 
-- This structure will be sent to your Options Load URL in case of 
-- using "External" data source for action. 
data OptionsLoad = OptionsLoad 
    { _loadName        :: T.Text 
    , _loadValue       :: T.Text 
    , _loadCallbackId  :: T.Text
    , _loadType        :: TypeOfInteraction
    , _loadTeamId      :: TeamId
    , _loadTeamDomain  :: T.Text
    , _loadChannelId   :: ChannelId
    , _loadChannelName :: T.Text
    , _loadUserId      :: UserId
    , _loadUserName    :: T.Text
    , _loadActionTs    :: POSIXTime
    , _loadMessageTs   :: POSIXTime
    , _loadAttachId    :: Int 
    , _loadToken       :: T.Text 
    } 
    deriving Show

-- | Data type for received message with user's choice. 
-- It also containes full original message to slack from bot. 
-- Full description can be optained here : 
-- https://api.slack.com/docs/interactive-message-field-guide#action_payload
data ReceivedInfo = ReceivedInfo 
    { _receivedType        :: TypeOfInteraction
    , _receivedActions     :: [ReceivedAction]
    , _receivedId          :: Maybe T.Text
    , _receivedTeamId      :: TeamId
    , _receivedTeamDomain  :: T.Text
    , _receivedChannelId   :: ChannelId
    , _receivedChannelName :: T.Text
    , _receivedUserId      :: UserId
    , _receivedUserName    :: T.Text
    , _receivedActionTs    :: POSIXTime
    , _receivedMessageTs   :: POSIXTime
    , _receivedToken       :: T.Text 
    , _receivedOriginal    :: OriginalMessage
    , _receivedURL         :: T.Text 
    }
    deriving Show


-- | Data type for information about user action (button click or choose from select list). 
data ReceivedAction = ReceivedS ReceivedSelect | ReceivedB ReceivedButton
    deriving Show

-- | Received data about select action. 
data ReceivedSelect = ReceivedSelect 
    { _rSelectName             :: T.Text 
    , _rSelectType             :: T.Text
    , _rSelectedOptions        :: [T.Text]
    } 
    deriving Show

-- | Received data about button click. 
data ReceivedButton = ReceivedButton
    { _rButtonName             :: T.Text 
    , _rButtonType             :: T.Text
    , _rButtonValue            :: T.Text
    }
    deriving Show

-- | Information about original message.
-- It includes info about attachments and actions.  
data OriginalMessage = OriginalMessage 
    { _orgText        :: T.Text
    , _orgBotId       :: BotId
    , _orgAttachments :: [OriginalAttachment]
    , _orgType        ::  T.Text
    , _orgSubtype     :: Maybe T.Text
    , _orgTs          :: POSIXTime 
    }
    deriving Show

-- | Attributes of attachment in the original message. 
data OriginalAttachment = OriginalAttachment
    { _orgCallback_id :: T.Text
    , _orgFallback    :: T.Text
    , _orgId          :: Int
    , _orgColor       :: Maybe AttachmentColor
    , _orgActions     :: Maybe [Action]  
    }
    deriving Show

-- | Type of interaction. 
-- Value of this type is inculded in responce from slack. 
data TypeOfInteraction = InteractiveMessage | DialogSubmission
    deriving (Show,Read) 

--- DEFAULT VALUES SECTION 

-- | Default value attachment type. 
defaultAttachment :: Attachment
defaultAttachment = Attachment
        { _attachmentFallback    = ""
        , _attachmentColor       = Just DefaultColor
        , _attachmentCallback_id = ""
        , _attachmentPretext     = Nothing
        , _attachmentAuthorName  = Nothing
        , _attachmentAuthorLink  = Nothing
        , _attachmentAuthorIcon  = Nothing
        , _attachmentTitle       = Nothing
        , _attachmentTitleLink   = Nothing
        , _attachmentText        = Nothing
        , _attachmentFields      = []
        , _attachmentImageUrl    = Nothing
        , _attachmentThumbUrl    = Nothing
        , _attachmentFooter      = Nothing
        , _attachmentFooterIcon  = Nothing
        , _attachmentTs          = Nothing
        , _attachmentActions     = []
        }

-- | Default value for button type. 
defaultButton :: Button
defaultButton = Button 
    { _buttonName             = ""
    , _buttonText             = ""
    , _buttonValue            = Nothing  
    , _buttonConfirm          = Nothing
    , _buttonData_Source      = Nothing 
    , _buttonMin_query_length = Nothing
    , _buttonStyle            = Nothing
    }

-- | Default value for Select type. 
defaultSelect :: Select
defaultSelect = Select
    { _selectName             = ""
    , _selectText             =  ""
    , _selectValue            = Nothing  
    , _selectConfirm          = Nothing
    , _selectData_Source      = Nothing 
    , _selectSelected_options = Nothing
    , _selectMin_query_length = Nothing
    , _selectOptions          = Nothing
    , _selectOption_groups    = Nothing
    }

-- | Default value for Confirm type.  
defaultConfirm = Confirm
    { _confirmTitle        = Nothing  
    , _confirmText         =  "" 
    , _confirmOk_text      = Nothing 
    , _confirmDismiss_text = Nothing 
    } 

-- | Default value for OptField type. 
defaultOptField = OptField 
    { _optFieldText        =  ""
    , _optFieldValue       = ""
    , _optFieldDescription = Nothing 
    }

-- | Default value for OptGroup type. 
defaultOptGroup = OptGroup 
    { _groupText    =  "" 
    , _groupOptions = []
    } 

--- INSTANCES SECTION 

makeLenses ''ChatMessage
makeLenses ''Button
makeLenses ''Select
makeLenses ''Attachment
makeLenses ''OptionsLoad 
makeLenses ''OptGroup 
makeLenses ''ReceivedInfo
makeLenses ''ReceivedAction
makeLenses ''OptField
makeLenses ''Confirm
makeLenses ''Field
makeLenses ''ReceivedButton
makeLenses ''ReceivedSelect
makeLenses ''OriginalMessage
makeLenses ''OriginalAttachment

instance ToJSON AttachmentColor where
    toEncoding x = toEncoding $ case x of
        DefaultColor  -> Nothing
        GoodColor     -> Just "good"
        WarningColor  -> Just "warning"
        DangerColor   -> Just "danger"
        CustomColor c -> Just c

instance FromJSON ChatMessage where
    parseJSON = withObject "ChatMessage" $ \o -> ChatMessage <$>
                            o .: "type" <*>
                            o .:? "user" <*>
                            defaultToEmpty o "text" <*>
                            o .: "ts"

instance ToJSON Action where 
    toJSON (ActionButton a) = toJSON a
    toJSON (ActionSelect a) = toJSON a 

instance FromJSON Action where
     parseJSON v = ActionButton <$> parseJSON v <|> ActionSelect <$> parseJSON v

instance FromJSON ReceivedAction where 
    parseJSON v = ReceivedS <$> parseJSON v <|> ReceivedB <$> parseJSON v

$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 8} ''Confirm)
$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 9} ''OptField)
$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 6} ''OptGroup)
$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 7}  ''MessagePayload)
$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 4}  ''PingPayload)
$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 11} ''Attachment)
$(deriveToJSON defaultOptions {fieldLabelModifier = toSnake . drop 6}  ''Field)
$(deriveFromJSON defaultOptions {fieldLabelModifier = toSnake . drop 6}  ''Field)
$(deriveFromJSON defaultOptions {fieldLabelModifier = toSnake . drop 11}  ''Attachment)
$(deriveFromJSON defaultOptions {fieldLabelModifier = toSnake . drop 8} ''Confirm)
$(deriveFromJSON defaultOptions {fieldLabelModifier = toSnake . drop 9} ''OptField)
$(deriveFromJSON defaultOptions {fieldLabelModifier = toSnake . drop 6} ''OptGroup)
$(deriveFromJSON defaultOptions {fieldLabelModifier = toSnake . drop 7} ''Button)
$(deriveFromJSON defaultOptions {fieldLabelModifier = toSnake . drop 7} ''Select)
$(deriveFromJSON defaultOptions {fieldLabelModifier = toSnake . drop 8} ''ReceivedButton)
$(deriveFromJSON defaultOptions {fieldLabelModifier = toSnake . drop 4} ''OriginalAttachment)

instance ToJSON DataSource where
    toJSON = String . T.toLower . T.pack . show  

instance ToJSON ButtonStyle where
    toJSON = String . T.toLower . T.pack . show 

instance ToJSON Select where 
    toJSON s = object $
        [ "name" .= (s  ^. selectName)
        , "text" .= (s  ^. selectText)
        , "type" .= String "select"
        ] ++ concat maybeOptions
        where  
            maybeOptions  = 
                [ "value"            .=. s ^. selectValue 
                , "data_source"      .=. s ^. selectData_Source
                , "selected_options" .=. s ^. selectSelected_options
                , "min_query_length" .=. s ^.  selectMin_query_length
                , "confirm"          .=. s ^. selectConfirm
                , "options"          .=. s ^. selectOptions
                , "option_groups"    .=. s ^. selectOption_groups
                ] 

instance ToJSON Button where 
    toJSON b = object $
        [ "name" .= (b  ^. buttonName)
        , "text" .= (b  ^. buttonText)
        , "type" .= String "button"
        ] ++ concat maybeOptions
        where  
            maybeOptions  = 
                [ "value"            .=. b ^. buttonValue 
                , "data_source"      .=. b ^. buttonData_Source
                , "min_query_length" .=. b ^. buttonMin_query_length
                , "confirm"          .=. b ^. buttonConfirm
                , "style"            .=. b ^. buttonStyle
                ] 


-- instance ToJSON Button where 

instance FromJSON ReceivedInfo where 
    parseJSON = withObject "payload" $ \o -> do
        _receivedType        <- o .: "type"
        _receivedActions     <- o .: "actions"
        _receivedId          <- optional $ o .: "callback_id"
        team                 <- o .: "team"
        _receivedTeamId      <- team .: "id"
        _receivedTeamDomain  <- team .: "domain" 
        channel              <- o .: "channel"
        _receivedChannelId   <- channel .: "id"
        _receivedChannelName <- channel .: "name"
        user                 <- o .: "user"
        _receivedUserId      <- user .: "id"
        _receivedUserName    <- user .: "name"
        _receivedActionTs    <- readPOSIXTime <$> o .: "action_ts"
        _receivedMessageTs   <- readPOSIXTime <$> o .: "message_ts" 
        _receivedToken       <- o .: "token"
        _receivedOriginal    <- o .: "original_message"
        _receivedURL         <- o .: "response_url"
        return ReceivedInfo{..}

instance FromJSON TypeOfInteraction where 
    parseJSON (String s) = case s of 
            "interactive_message" -> return InteractiveMessage
            "dialog_submission"   -> return DialogSubmission
            _                     -> return InteractiveMessage

instance FromJSON DataSource where 
    parseJSON (String s) = case s of 
            "static"        -> return Static 
            "users"         -> return Users 
            "channels"      -> return Channels 
            "conversations" -> return Conversations 
            "external"      -> return External
            _               -> return Static

instance FromJSON ButtonStyle where 
    parseJSON (String s) = case s of 
        "default" -> return Default 
        "primary" -> return Primary
        "danger"  -> return Danger
        _         -> return Default

instance FromJSON ReceivedSelect where 
    parseJSON = withObject "ReceivedAction" $ \o -> do
           _rSelectName              <- o .: "name"
           _rSelectType              <- o .: "type"
           selected                  <- o .: "selected_options"
           _rSelectedOptions         <- mapM (flip (.:) "value") selected
           return ReceivedSelect{..} 



instance FromJSON AttachmentColor where 
    parseJSON (String s) = case s of 
        "default" -> return DefaultColor       
        "good"    -> return GoodColor
        "warning" -> return WarningColor
        "danger"  -> return DangerColor       
        custom    -> return $ CustomColor custom 


instance FromJSON OriginalMessage where 
    parseJSON = withObject "OriginalMessage" $ \o -> do
        _orgText        <- o .: "text" 
        _orgBotId       <- o .: "bot_id"
        _orgAttachments <- o .: "attachments"
        _orgType        <- o .: "type" 
        _orgSubtype     <- optional $ o .: "subtype"
        _orgTs          <- readPOSIXTime <$> o .: "ts" 
        return OriginalMessage{..}

instance FromJSON OptionsLoad where 
    parseJSON = withObject "OptionsLoad" $ \o -> do 
        _loadName         <- o .: "name"
        _loadValue        <- o .: "value"
        _loadCallbackId   <- o .: "callback_id"
        _loadType         <- o .: "type"
        team              <- o .: "team"
        _loadTeamId       <- team .: "id"
        _loadTeamDomain   <- team .: "domain" 
        channel           <- o .: "channel"
        _loadChannelId    <- channel .: "id"
        _loadChannelName  <- channel .: "name"
        user              <- o .: "user"
        _loadUserId       <- user .: "id"
        _loadUserName     <- user .: "name"
        _loadActionTs     <- readPOSIXTime <$> o .: "action_ts"
        _loadMessageTs    <- readPOSIXTime <$> o .: "message_ts"
        _loadAttachmentId <- read @Int <$> o .: "attachment_id"
        _loadToken        <- o .: "token"
        return OptionsLoad{..}

-- | helper function for reading POSIXTime from String. 
readPOSIXTime :: String -> POSIXTime
readPOSIXTime = realToFrac . read @Double 

-- | Function, that helps to avoid creation fields for Nothing values, instead of creation fields 
-- with nulls. 
(.=.) :: (ToJSON a, KeyValue kv) => T.Text -> Maybe a -> [kv]
(.=.) _ Nothing = []
(.=.) name c = [name .= c]

infixl 7 .=.