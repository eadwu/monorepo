use futures_util::StreamExt;

pub struct Mercenary {
    identifier: uuid::Uuid,
    client: async_nats::Client,
}

struct MercenaryChannel {
    topic: String,
    queue_group: String,
}

impl Mercenary {
    pub fn new(nc: async_nats::Client) -> Mercenary {
        let id = uuid::Uuid::new_v4();
        Mercenary {
            identifier: id,
            client: nc,
        }
    }

    fn nats_client(&self) -> &async_nats::Client {
        &self.client
    }

    fn topic(&self) -> String {
        format!("{}.{}", guild::GUILD_MERCENARY_TOPIC, &self.identifier)
    }

    pub async fn routine(&self) -> Result<(), async_nats::Error> {
        tracing::info!(
            "Mercenary `{}` has been recruited for operations",
            &self.identifier
        );

        let communication_channels = [
            // Quest Board (broadcast)
            MercenaryChannel::new(
                guild::GUILD_QUEST_BOARD_TOPIC,
                guild::GUILD_ALL_MERCENARY_QUEUE_GROUP,
            ),
            // Direct Communication (unicast)
            MercenaryChannel::new(self.topic(), self.identifier.to_string()),
        ];

        let mut handles = Vec::with_capacity(communication_channels.len());
        for channel in communication_channels {
            tracing::debug!(
                "Mercenary `{}` is listening on topic `{}` (group `{}`)",
                &self.identifier,
                &channel.topic,
                &channel.queue_group
            );
            handles.push(tokio::spawn(Mercenary::handler(
                self.nats_client().clone(),
                channel.topic,
                channel.queue_group,
            )))
        }
        for handle in handles {
            handle.await??
        }
        Ok(())
    }

    async fn handler<T: Into<String>>(
        nats_client: async_nats::Client,
        topic: T,
        queue_group: T,
    ) -> Result<(), async_nats::Error> {
        let mut subscription = nats_client
            .queue_subscribe(topic.into(), queue_group.into())
            .await?;

        while let Some(quest_msg) = subscription.next().await {
            println!("{:?}", quest_msg);
        }
        Ok(())
    }
}

impl MercenaryChannel {
    fn new<T: Into<String>>(topic: T, queue_group: T) -> MercenaryChannel {
        MercenaryChannel {
            topic: topic.into(),
            queue_group: queue_group.into(),
        }
    }
}
