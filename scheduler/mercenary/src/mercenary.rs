pub struct Mercenary {
    identifier: uuid::Uuid,
}

impl Mercenary {
    pub fn new() -> Mercenary {
        let id = uuid::Uuid::new_v4();
        Mercenary { identifier: id }
    }

    pub async fn routine(self) -> Result<(), async_nats::Error> {
        tracing::info!(
            "Mercenary `{}` has been recruited for operations",
            &self.identifier
        );

        Ok(())
    }
}
