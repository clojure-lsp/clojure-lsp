(comment
  (let [db (im.db/_d)
        user-id (im.models.user.queries/user-by-email "alice@example.com")
        user (datomic.api/entity db user-id)
        permissions (im.models.permission.queries/all-user-perms-as-map user)]
    permissions))
